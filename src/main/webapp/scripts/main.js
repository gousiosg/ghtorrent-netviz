$(function() {

    var prefix = "/"
    var color = d3.scale.category10();
    var colormap = {};
    var btnmap = {};
    var selected = {};
    var graph = {};
    var graphTrans = [0,0],
        graphScale = 1;

    $.getJSON(prefix + 'langs', function(data) {
         var langs = data.map(function(x){ return x.name;})

         $("#langsearch").autocomplete ({
                source: langs,
                minLength: 1,
                delay : 100,
                focus: function( event, ui ) {},
                select: function( event, ui ) {
                    $("#langsearch").val("");
                    createLangButton(ui.item.label, $('#selected-languages'));
                    updateGraph(Object.keys(colormap));
                    return false;
                }
            });
         return langs;
    });

    function hexToRgb(hex) {
        var result = /^#?([a-f\d]{2})([a-f\d]{2})([a-f\d]{2})$/i.exec(hex);
        return result ? {
            r: parseInt(result[1], 16),
            g: parseInt(result[2], 16),
            b: parseInt(result[3], 16)
        } : null;
    }

    function createLangButton(lang, appendTo) {
        var c = colormap[lang];

        if ( !c ) {
            c = color(lang);
            colormap[lang] = c;
            var rgb = hexToRgb(c);

            btnmap[lang] = $('<span/>',{
                class: 'langLabel',
                style: 'background: rgba(' + rgb.r + ',' + rgb.g +',' + rgb.b+ ', 0.3)',
                id: 'lang-' + lang
            }).append(
                (lang) + '<span class="lang-remove">&#10006;</span>'
            ).appendTo( appendTo )[0];

            $('#lang-' + lang + '> span.lang-remove').click(function(){
                $(this).parent().remove();
                delete colormap[lang];
                updateGraph(Object.keys(colormap));
            });
        }
        return c;
    };

    // Create the d3.js canvas
    var width = $(window).width() - 20,
        height = $(window).height() - 20;

    var zoomer = d3.behavior.zoom();

    var svg = d3.select("#graph")
                .append("svg")
                .attr("width", width)
                .attr("height", height);

    function onzoom() {
        graphScale = zoomer.scale();
        graphTrans = zoomer.translate();
        d3.selectAll(".node").attr("transform",
            "translate("+graphTrans+")"+" scale("+graphScale+")");
        d3.selectAll(".link").attr("transform",
            "translate("+graphTrans+")"+" scale("+graphScale+")");
    }

    function updateGraph(langs) {
        var tmp = langs.reduce(function(acc, x){return acc + "l=" + x + "&" ;},"");
        var q = tmp.substring(0, tmp.lastIndexOf('&'));
        q = q + "&m=rank"
        d3.json(prefix + "links?" + q, function(error, g) {

            graph = g;

            d3.select("#graph > svg > g").remove();
            d3.select("#totalNodesLabel").text(g.nodes.length);
            d3.select("#totalLinksLabel").text(g.links.length);

            // Define the plotting area
            var plot = svg.append('g')
                          .call(zoomer.on("zoom", onzoom ));

            var force = d3.layout.force()
                        .charge(-220)
                        .gravity(0.2)
                        .linkDistance(300)
                        .size([width, height]);

            force.nodes(graph.nodes)
                 .links(graph.links);

            var link = plot.selectAll(".link")
                          .data(graph.links)
                          .enter()
                          .append("line")
                          .attr("class", "link")
                          .style("stroke-width", 1)
                          .on("mouseover", linkMouseover)
                          .on("mouseout", mouseout);

            var node = plot.selectAll(".node")
                          .data(graph.nodes)
                          .enter()
                          .append("circle")
                          .attr("class", "node")
                          .attr("r", function(d){ var rank = d.rank * 1000.0; if (rank > 0) {return rank;} else {return 2;}})
                          .style("fill", function(d) { return colormap[d.lang]; })
                          .on("click", nodeClick)
                          .on("mouseover", nodeMouseover)
                          .on("mouseout", mouseout);

            node.append("title").text(function(d) { return d.name; });

            force.on("tick", function() {
                link.attr("x1", function(d) { return d.source.x; })
                    .attr("y1", function(d) { return d.source.y; })
                    .attr("x2", function(d) { return d.target.x; })
                    .attr("y2", function(d) { return d.target.y; });

                node.attr("cx", function(d) { return d.x; })
                    .attr("cy", function(d) { return d.y; });
            });

            force.start();
        });
    }

    function linkMouseover(d) {
        svg.selectAll(".link").classed("active", function(p) { return p === d; });
        svg.selectAll(".node").classed("active", function(p) { return p === d.source || p === d.target; });
      }

      // Highlight the node and connected links on mouseover.
      function nodeMouseover(d) {
        svg.selectAll(".link").classed("active", 
            function(p) { return p.source === d || p.target === d; });
        d3.select(this).classed("active", true);
        d3.select("#lang-" + d.lang).classed("active", true);
      }

      // Clear any highlighted nodes or links.
      function mouseout() {
        d3.selectAll(".active").classed("active", false);
      }

    function nodeClick(n) {

        d3.json(prefix + "project?p=" + n.pid, function(error, p) {

            var x = (n.x * graphScale) + graphTrans[0];
            var y = (n.y * graphScale) + graphTrans[1];

            var owner = p.name.split("/")[0];
            var project = p.name.split("/")[1];
            var url = "http://github.com/" + p.name;

            showPopup( "Project: "  + project,
                      ["Language: " + (n.lang || UNKNOWN),
                       "Owner: "    + owner,
                       "Common devs with: "    + graph.links.filter(function(x){return (x.source == n || x.target == n);}).length + " projects",
                       "Rank: "     + n.rank,
                       "Url: <a target=\"_blank\" href=\"" + url + "\">" + url + "</a>"], 
                      [x,y]);
        });

        showPopup
    }

    function showPopup(title,contents,pos) {
        $("#pop-up").fadeOut(100,function () {
            // Popup content
            $("#pop-up-title").html(title);
            $("#pop-up-content").html( "" );
            for (var i = 0; i < contents.length; i++) {
                $("#pop-up-content").append("<div>"+contents[i]+"</div>");
            }
            // Popup position
            var popLeft = pos[0]+20;
            var popTop  = pos[1]+20;
            $("#pop-up").css({"left":popLeft,"top":popTop});
            $("#pop-up").fadeIn(100);
        });
    }

    $("#pop-up").mouseleave(function(e){
        $("#pop-up").fadeOut(50);
    })

    function hidePopup() {
        eff = $("#pop-up").delay(100).fadeOut(50);
        d3.select(this).attr("fill","url(#ten1)");
    }
});

