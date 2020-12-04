window.onload = function(){

var svgCanvas = d3.select("svg")
.attr("width", 1000)
.attr("height", 600)
.attr("class", "svgCanvas");

var nodes = d3.select("body").append("div")
.attr("class", "tooltip");

d3.json("data.json", function(data) {
    console.log(data);
    
function Number(node) 
{
 var num = 0;
 for (var i = 0; i < data.links.length; i++) 
 {
if (data.links[i].node01 == node || data.links[i].node02 == node )
num = num + 1; 
 }
 return num; 
}
    
function Amount(node) 
{
 var amt = 0;
 for (var i = 0; i < data.links.length; i++) 
 {
    if (data.links[i].node01 == node || data.links[i].node02 == node)
    amt = amt + data.links[i].amount; 
 }
 return amt; 
}
    
    
function xpos(node) 
{
 for (var i = 0; i < data.nodes.length; i++) 
 {
    if (data.nodes[i].id == node)
    return data.nodes[i].x; 
 }
}

    
function ypos(node) 
{
 for (var i = 0; i < data.nodes.length; i++) 
 {
    if (data.nodes[i].id == node)
    return data.nodes[i].y; 
 }
}
    
 svgCanvas.selectAll("links")
 .data(data.links) 
 .enter()
 .append("line")
 .attr("x1", function(d) { return xpos(d.node01);})
 .attr("y1", function(d) { return ypos(d.node01);})
 .attr("x2", function(d) { return xpos(d.node02);})
 .attr("y2", function(d) { return ypos(d.node02);})
 .style("stroke", "darkblue")
 .attr("stroke-width", function(d) { return (d.amount/50); })


svgCanvas.selectAll("location")
.data(data.nodes)
.enter()
.append("circle")
.attr("cx", function(d) { return d.x; })
.attr("cy", function(d) { return d.y; })
.attr("r", function(d) { return (Amount(d.id)/35);})
.attr("fill", "cyan")
    
.on("mouseover", function(d){
    var id = d.id;
    svgCanvas.selectAll("circle")
    .attr("opacity", 0.2);               
    d3.select(this)                          
    .attr("opacity", 1);
    svgCanvas.selectAll("line")
    .attr("opacity",0);
    data.links.forEach(function(){
    svgCanvas.selectAll("line")
    .attr("opacity", function(link){
             if(link.node01 == id || link.node02 == id)
                 return 1;
             else return 0; 
         });
    }); 
     
    nodes.transition()	
    .style("opacity", 0.80);
    nodes.html("<p>Total amount of trading: "+Amount(d.id)+"</p>"+"<p>Number of Connected Locations: "+Number(d.id)+"</p>")
    .style("left", (d3.event.pageX) + "px")	
    .style("top", (d3.event.pageY-25) + "px");           
})

.on("mouseout", function(d){
    svgCanvas.selectAll("circle")                                                 
    .attr("opacity", 1);
    svgCanvas.selectAll("line") 
    .attr("opacity", 1);
    nodes.transition()	
    .style("opacity", 0);
});

svgCanvas.selectAll("text")
.data(data.nodes)
.enter()
.append("text")
.attr("x", function(d){return d.x;})
.attr("y", function(d){return d.y;})
.attr("text-anchor", "middle")
.attr("fill", "darkblue")
.text(function(d){ return d.id;});

    
});
}