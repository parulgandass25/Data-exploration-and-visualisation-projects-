window.onload = function(){
    var svgCanvas = d3.select("svg")
        .attr("width",960)
        .attr("height",540)
        .attr("class","svgCanvas");

    svgCanvas.append("rect")
        .attr("x",100)
        .attr("y",100)
        .attr("width",100)
        .attr("height",50)
        .attr("rx",20)
        .attr("ry",10)
        .attr("fill","lightblue"); 

    svgCanvas.append("ellipse")
        .attr("cx",300)
        .attr("cy",400)
        .attr("rx",30)
        .attr("ry",60)
        .attr("fill","yellowgreen"); }