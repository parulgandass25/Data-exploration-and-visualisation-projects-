window.onload = function(){
    var svgCanvas = d3.select("svg")
        .attr("width",960)
        .attr("height",540)
        .attr("class","svgCanvas");         
    
    d3.csv("third.csv",function(d){
        console.log(d); 
//    
        var minValue = Infinity;
            var maxValue = -1;

        d.forEach(function(thisD){
            var thisValue = thisD["value"];
            minValue = Math.min(minValue,thisValue);
            maxValue = Math.max(maxValue, thisValue);
        });

    var value2range = d3.scaleLinear()
    .domain([minValue, maxValue])
    .range([0.5,1]);

    var range2color = d3.interpolateBlues;
           
    svgCanvas.selectAll("circles")
        .data(d).enter()
        .append("circle")
        .attr("cx", function(thisElement, index){
        return 150+index*150;
        })
            .attr("cy",300)
            .attr("r",function(thisElement, index){
            return thisElement["value"];
        })
    
        .attr("fill",function(thisElement, index){
            return range2color(value2range(thisElement["value"]));
        }).
        on("mouseover",function(thisElement,index){
            svgCanvas.selectAll("circle").attr("opacity",0);
            d3.select(this)
                .attr("opacity",1);})
                .on("mouseout",function(thisElement,index){
            svgCanvas.selectAll("circle").attr("opacity",1);
        });
////
        svgCanvas.selectAll("text")
            .data(d).enter()
            .append("text")
            .attr("x",function(thisElement, index){
            return 150 +index*150;
        })
            .attr("y", 300-35)
            .attr("text-anchor","middle")
            .text(function(thisElement,index){
            return thisElement["title"] + ": "+thisElement["value"];
        });         
     });
}