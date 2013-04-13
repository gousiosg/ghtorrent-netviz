$(function() {

    var color = d3.scale.category20c();
    var colormap = {};
    var btnmap = {};
    var selected = {};
    var strict = true;

    var langs = $.getJSON('/langs', function(data) {
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


    function createLangButton(lang, appendTo) {
        var c = colormap[lang];

        if ( !c ) {
            c = color(lang);
            colormap[lang] = c;

            btnmap[lang] = $('<span/>',{
                class: 'langLabel',
                style: 'background: '+ c +';',
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

    // Create d3.js canvas
    var width = $(window).width() - 20,
        height = $(window).height() - 20;

    var force = d3.layout.force()
        .charge(-120)
        .linkDistance(30)
        .size([width, height]);

    var svg = d3.select("#graph").append("svg")
        .attr("width", width)
        .attr("height", height);

    function updateGraph(langs) {
        var tmp = langs.reduce(function(acc, x){return acc + "l=" + x + "&" ;},"");
        var q = tmp.substring(0, tmp.lastIndexOf('&'));
        d3.json("/links?" + q, function(error, graph) {
          force.nodes(graph.nodes)
               .links(graph.links)
               .start();

          var link = svg.selectAll(".link")
              .data(graph.links)
              .enter().append("line")
              .attr("class", "link")
              .style("stroke-width", function(d) { return Math.sqrt(d.value); });

          var node = svg.selectAll(".node")
              .data(graph.nodes)
            .enter().append("circle")
              .attr("class", "node")
              .attr("r", 5)
              .style("fill", function(d) { return colormap[d.lang.name]; })
              .call(force.drag);

          node.append("title")
              .text(function(d) { return d.name; });

          force.on("tick", function() {
            link.attr("x1", function(d) { return d.source.x; })
                .attr("y1", function(d) { return d.source.y; })
                .attr("x2", function(d) { return d.target.x; })
                .attr("y2", function(d) { return d.target.y; });

            node.attr("cx", function(d) { return d.x; })
                .attr("cy", function(d) { return d.y; });
          });
        });
    }

});

