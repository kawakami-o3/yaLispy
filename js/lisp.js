$(function () {
//  $("#log").append("<span></span>").find("span").append("<div>hello</div").end();

  var to_string = function(exp) {
    return exp;
  };

  $("#code").keydown(function(e) {
    if (e.keyCode == 13) {
      var i = 1;
      $("#log").prepend("> " + i.class + "<br/>");
      $("#log").prepend("INPUT> " + $("#code").val() + "<br/>");
      if ($("#code").val() !== "") {
        $("#log").prepend("-> " + to_string($("#code").val()) + "<br/>");
      }
      $("#code").attr("value","");
    }
    return true;
  });

});

