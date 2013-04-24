$(function() {

    var prefix = "/netviz/"
    var color = d3.scale.category10();
    var colormap = {};
    var btnmap = {};
    var selected = {};
    var graph = {};
    var hist = {};
    var selectedHist; resetHistSelection();
    var graphTrans = [0,0],
        graphScale = 1;
    var graphSVG, zoomer;
    var resizeTimer;
    
    // Adapt the graph canvas to window resizes
    $(window).resize(function() {
        clearTimeout(resizeTimer);
        resizeTimer = setInterval(function(){
            width = $(window).width() - 20;
            height = $(window).height() - 20;
            d3.select("#graph > svg")
                .attr("width", width)
                .attr("height", height);
            updateHistogram(Object.keys(colormap));
            clearTimeout(resizeTimer);
        }, 200);
    });

    // Get values for the language autocomplete box
    $.getJSON(prefix + 'langs', function(data) {
         var langs = data.map(function(x){ return x.name;})

         $("#langsearch").autocomplete ({
                source: langs,
                minLength: 1,
                delay : 100,
                focus: function( event, ui ) {},
                select: function( event, ui ) {
                    $("#langsearch").val("");
                    createLangButton(ui.item.label.toLowerCase(), $('#selected-languages'));
                    update(Object.keys(colormap));
                    return false;
                }
            });
         return langs;
    });

    $("#algo").change(function(){
        updateGraph(Object.keys(colormap));
    });

    $("#edge").change(function(){
        updateGraph(Object.keys(colormap));
    });

    // Convert an HTML hex colour to an RGB triplette
    function hexToRgb(hex) {
        var result = /^#?([a-f\d]{2})([a-f\d]{2})([a-f\d]{2})$/i.exec(hex);
        return result ? {
            r: parseInt(result[1], 16),
            g: parseInt(result[2], 16),
            b: parseInt(result[3], 16)
        } : null;
    }

    // On select a language from the JQuery drop down add a language button
    // to the language button div. Setup a handler for when close is clicked.
    function createLangButton(lang, appendTo) {
        var c = colormap[lang];

        if ( !c ) {
            c = color(lang);
            colormap[lang] = c;
            var rgb = hexToRgb(c);

            btnmap[lang] = $('<span/>',{
                class: 'langLabel ' + lang.toLowerCase(),
                style: 'background: rgba(' + rgb.r + ',' + rgb.g +',' + rgb.b+ ', 0.8)',
                id: 'lang-' + lang
            }).append(
                (lang) + '<span class="lang-remove">&#10006;</span>'
            ).appendTo(appendTo)[0];

            $('#lang-' + lang + '> span.lang-remove').click(function(){
                $(this).parent().remove();
                delete colormap[lang];
                update(Object.keys(colormap));
            });

            $('#lang-' + lang).mouseover(function() {
                var lang = $(this).attr('class').split(/\s+/)[1];
                d3.selectAll('.' + lang).classed('active', true).style('stroke', colormap[lang]);
            });

            $('#lang-' + lang).mouseout(function() {
                var lang = $(this).attr('class').split(/\s+/)[1];
                d3.selectAll('.' + lang).classed('active', false).style('stroke', "#fff");
            });
        }
        return c;
    };

    function update(langs) {
        updateGraph(langs);
        updateHistogram(langs);
    }

    // Scale links and nodes to the new zoom level, using CSS transformations
    function onzoom() {
        graphScale = zoomer.scale();
        graphTrans = zoomer.translate();

        d3.selectAll(".node > circle").attr("transform",
            "translate("+graphTrans+")"+" scale("+graphScale+")");
        d3.selectAll(".node > text").attr("transform",
            "translate("+graphTrans+")"+" scale("+graphScale+")");
        d3.selectAll(".link").attr("transform",
            "translate("+graphTrans+")"+" scale("+graphScale+")");
    }

    function formatLangReqURL(langs, from, to) {
        var from = from || selectedHist.start || 0;
        var to = to || selectedHist.end || (Math.pow(2,32) - 1);

        if (from == to){
            from = 0;
            to = (Math.pow(2,32) - 1);
        }

        var tmp = langs.reduce(function(acc, x){return acc + "l=" + encodeURIComponent(x) + "&" ;},"");
        var q = tmp + "f=" + from + "&t=" + to;
        return q;
    }

    // Scale links and nodes to the new zoom level, using CSS transformations
    function updateGraph(langs) {
        $("#project-search").css('visibility', 'hidden');
        d3.select(".graph").remove();
        d3.select("#totalNodesLabel").text(0);
        d3.select("#totalLinksLabel").text(0);

        if (langs.length == 0)
            return;

        var edges = edges || $("#edge").val() || 5000;
        var algo = algo || $("#algo").val() || "rank";
        var q = formatLangReqURL(langs) + "&m=" + algo + "&e=" + edges;

        d3.json(prefix + "links?" + q, function(error, g) {
            g.nodes.sort(function(a,b){return b.rank - a.rank;})
            graph = g;
            updateNodeSearch();

            d3.select("#totalNodesLabel").text(g.nodes.length);
            d3.select("#totalLinksLabel").text(g.links.length);

                // Create the d3.js graph canvas
            var width = $(window).width() - 20,
                height = $(window).height() - 20;

            zoomer = d3.behavior.zoom();

            graphSVG = d3.select("#graph")
                            .append("svg")
                            .attr("width", width)
                            .attr("height", height)
                            .call(zoomer.on("zoom", onzoom ));

            graphSVG.classed("graph", true);

            var force = d3.layout.force()
                        .charge(-220)
                        .gravity(0.2)
                        .friction(0.6)
                        .linkDistance(250)
                        .size([width, height]);

            force.nodes(graph.nodes)
                 .links(graph.links);

            var link = graphSVG.selectAll(".link")
                          .data(graph.links)
                          .enter()
                          .append("line")
                          .attr("class", "link")
                          .style("stroke-width", 1)
                          .on("mouseover", linkMouseover)
                          .on("mouseout", mouseout);

            var node = graphSVG.selectAll(".node")
                          .data(graph.nodes)
                          .enter()
                          .append("g")
                          .attr("class", "node")

            var nodes = node.append("circle")
                  .attr("r", function(d){return radius(d);})
                  .style("fill", function(d) { return colormap[d.lang.toLowerCase()]; })
                  .attr("class", function(d) { return d.lang.toLowerCase(); })
                  .on("click", nodeClick)
                  .on("mouseover", nodeMouseover)
                  .on("mouseout", mouseout)
                  .call(force.drag);

            node.append("title").text(function(d) { return d.name; });

            var lastNode = g.nodes.slice(0,10).last();
            var maxRank;
            if (typeof lastNode === "undefined")
                maxRank = 0;
            else
                maxRank = lastNode.rank;

            var labels = node.filter(function(d){ return (d.rank >= maxRank);})
                .append("text")
                .attr("text-anchor", "middle")
                .text(function(d){return d.name;})
                .attr("class", "nodename");


            force.on("tick", function() {
                labels.attr("x", function(d) { return d.x; })
                      .attr("y", function(d) { return d.y; });

                nodes.attr("cx", function(d) { return d.x; })
                      .attr("cy", function(d) { return d.y; });

                link.attr("x1", function(d) { return d.source.x; })
                    .attr("y1", function(d) { return d.source.y; })
                    .attr("x2", function(d) { return d.target.x; })
                    .attr("y2", function(d) { return d.target.y; });
            });

            function radius(d) {
                var rank = d.rank * 1000.0;
                if (rank > 0)
                    return rank;
                else
                    return 2;
            }

            force.start();
        });
    }

    function linkMouseover(d) {
        graphSVG.selectAll(".link").classed("active", function(p) { return p === d; });
        graphSVG.selectAll(".node").classed("active", function(p) { return p === d.source || p === d.target; });
      }

      // Highlight the node and connected links on mouseover.
      function nodeMouseover(d) {
        graphSVG.selectAll(".link").classed("active", 
            function(p) { return p.source === d || p.target === d; });
        d3.select(this).classed("active", true);
        d3.select("#lang-" + d.lang).classed("active", true);
      }

      // Clear any highlighted nodes or links.
      function mouseout() {
        d3.selectAll(".active").classed("active", false);
      }

    function nodeClick(n) {

            var x = (n.x * graphScale) + graphTrans[0];
            var y = (n.y * graphScale) + graphTrans[1];

            var owner = n.name.split("/")[0];
            var project = n.name.split("/")[1];
            var url = "http://github.com/" + n.name;

            showPopup( "Project: "  + project,
                      ["Language: " + (n.lang || UNKNOWN),
                       "Owner: "    + owner,
                       "Common devs with: "    + graph.links.filter(function(x){return (x.source == n || x.target == n);}).length + " projects",
                       "Rank: "     + n.rank,
                       "Url: <a target=\"_blank\" href=\"" + url + "\">" + url + "</a>"], 
                      [x,y]);

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

    function updateNodeSearch() {
        $("#project-search").css('visibility', 'visible');

        $("#projectsearch").autocomplete ({
                source: graph.nodes.map(function(x){return x.name;}),
                minLength: 3,
                delay : 100,
                focus: function( event, ui ) {
                    highlight(ui.item.label);
                },
                select: function( event, ui ) {
                    $("#projectsearch").val("");
                    highlight(ui.item.label);
                    return false;
                }
            });

        function highlight(project) {
            var node = graph.nodes.filter(function(x){return x.name == project;});
            graphSVG.selectAll(".link").classed("active", 
                function(p) { return p.source === node[0] || p.target === node[0]; });
        }
    }

    function updateHistogram(languages) {
        d3.select("#hist > svg").remove();

        if(languages.length == 0)
            return;
        var q = formatLangReqURL(languages);

        d3.json("hist?" + q, function(error, data) {
            resetHistSelection();
            var margin = {top: 3, right: 60, bottom: 20, left: 50},
                width = $("#hist").width() - margin.right,
                height = $("#hist").height() - margin.bottom;

            // x axis configuration
            var x = d3.time.scale().range([0, width]);
            var xAxis = d3.svg.axis()
                .scale(x)
                .orient("bottom");
            x.domain(d3.extent(data, function(d) { return d.date; }));

            // y axis configuration
            var y = d3.scale.linear().range([height, 0]);
            var yAxis = d3.svg.axis()
                .scale(y)
                .orient("left")
                .ticks(4);
            var commitsPerDate = d3.values(data.reduce(function(acc, x){
                                            if(x.date in acc){acc[x.date] += x.count;}
                                            else {acc[x.date] = x.count}; 
                                            return acc;}, {}));
            y.domain([0, d3.max(commitsPerDate)]);

            // Plot area configuration
            var svg = d3.select("#hist").append("svg")
                        .attr("width", width + margin.left + margin.right)
                        .attr("height", height + margin.top + margin.bottom)
                        .append("g")
                        .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

            var brush = d3.svg.brush()
                        .x(x)
                        .on("brushend", brushend)
                        .extent([selectedHist.start, selectedHist.end]);    

            // Create a stack of plot data indexed by language name
            var stack = d3.layout.stack().values(function(d) { return d.values; });
            var langs = stack(languages.map(function(lang){
                return {
                    name: lang.toLowerCase(),
                    values: data.filter(function(x){ return x.lang.toLowerCase() == lang.toLowerCase()}).map(function(d){
                        return {
                            date: d.date,
                            y: d.count
                        }
                    })
                }
            }));

            var area = d3.svg.area()
                        .x(function(d) { return x(d.date); })
                        .y0(function(d) { return y(d.y0); })
                        .y1(function(d) { return y(d.y0 + d.y); });

            var lang = svg.selectAll(".browser")
                  .data(langs)
                  .enter().append("g")
                  .attr("class", "browser");

            lang.append("path")
                  .attr("class", "area")
                  .attr("d", function(d) { return area(d.values); })
                  .style("fill", function(d) { return colormap[d.name.toLowerCase()]; });

            svg.append("g")
                  .attr("class", "x axis")
                  .attr("transform", "translate(0," + height + ")")
                  .call(xAxis);

            svg.append("g")
                  .attr("class", "y axis")
                  .call(yAxis)
                  .append("text")
                  .attr("transform", "rotate(-90)")
                  .attr("y", 6)
                  .attr("dy", ".71em")
                  .style("text-anchor", "end")
                  .text("commits");

            svg.append("g")
               .attr("class", "brush")
               .call(brush)
               .selectAll("rect")
               .attr("y", -6)
               .attr("height", height + 7);    

            function brushend() {
                var e = brush.extent();
                var from = Math.round(e[0].getTime() / 1000)
                    to = Math.round(e[1].getTime() / 1000);

                if (from != to)
                    selectedHist = {
                        start: from,
                        end: to  
                    }
                else
                    resetHistSelection();
                
                updateGraph(languages);
            }
        });
    }

    function resetHistSelection(){
        selectedHist = {
            start: 1325376000000,
            end:   1356998400000
        };
    }
});

Array.range= function(a, b, step){
    var A= [];
    if (typeof a == 'number') {
        A[0] = a;
        step = step || 1;
        while(a + step <= b){
            A[A.length] = a+= step;
        }
    }
    else {
        var s = 'abcdefghijklmnopqrstuvwxyz';
        if(a === a.toUpperCase()){
            b = b.toUpperCase();
            s = s.toUpperCase();
        }
        s = s.substring(s.indexOf(a), s.indexOf(b)+ 1);
        A = s.split('');        
    }
    return A;
}

Array.prototype.last = function() {
    return this[this.length - 1];
}


