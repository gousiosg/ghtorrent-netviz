$(function() {

    var color = d3.scale.category10();
    var colormap = {};
    var btnmap = {};
    var selected = {};
    var strict = true;
    var graphTrans = [0,0],
        graphScale = 1;

    $.getJSON('/langs', function(data) {
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
        d3.select("#graph > svg > g").attr("transform",
            "translate("+graphTrans+")"+" scale("+graphScale+")");
    }

    function updateGraph(langs) {
        var tmp = langs.reduce(function(acc, x){return acc + "l=" + x + "&" ;},"");
        var q = tmp.substring(0, tmp.lastIndexOf('&'));

        d3.json("/links?" + q, function(error, graph) {

            // Define the plotting area
            var plot = svg.append('g')
                          .call(zoomer.on("zoom", onzoom ));

            var force = d3.layout
                        .force()
                        .charge(-120)
                        .linkDistance(30)
                        .size([width, height]);

            force.nodes(graph.nodes)
               .links(graph.links);

            var link = plot.selectAll(".link")
                          .data(graph.links)
                          .enter()
                          .append("line")
                          .attr("class", "link")
                          .style("stroke-width", function(d) { return Math.sqrt(d.value);});

            var node = plot.selectAll(".node")
                          .data(graph.nodes)
                          .enter()
                          .append("circle")
                          .attr("class", "node")
                          .attr("r", function(d){ return Math.log(d.commits)})
                          .style("fill", function(d) { return colormap[d.lang]; })
                          .on("click", showNodePopup);
                          //.call(force.drag);

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

    function showNodePopup(n) {

        d3.json("/project?p=" + n.pid, function(error, p) {

            var x = (n.x * graphScale) + graphTrans[0];
            var y = (n.y * graphScale) + graphTrans[1];

            var owner = p.name.split("/")[0];
            var project = p.name.split("/")[1];
            var url = "http://github.com/" + p.name;

            showPopup( "Project: "  + project,
                      ["Language: " + (n.lang || UNKNOWN),
                       "Owner: "    + owner,
                       "Url: <a target=\"_blank\" href=\"http://github.com/" + url + "\">" + url + "</a>"], 
                      [x,y]);
        });
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

