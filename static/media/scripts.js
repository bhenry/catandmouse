$(function() {

  $("div.card").click(function(e) {
    
    if (!$(e.target).is('input')) {
      $(this).find('input[name="playme"]').attr("checked",true);
    }
  });

});
