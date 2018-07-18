$( document ).ready(function() {
  
  function handleOutboundLinkClicks(event) {
    if(event.context === undefined){
        ga('send', 'event', {
          eventCategory: 'Outbound Link',
          eventAction: 'click',
          eventLabel: event.target.text,
          transport: 'beacon'
        });
    }else{
      ga('send', 'event', {
          eventCategory: 'Outbound Link',
          eventAction: 'click',
          eventLabel: event.context.text,
          transport: 'beacon'
        });
    }
  }

  $('#aoa_links a h3').click(function() {
	  $('.navbar-nav li').removeClass();
    var clicked = $(this).text();
    $('.navbar-nav li a').each(function(index) {
	    if($(this).attr('data-value')==clicked){
		    handleOutboundLinkClicks($(this).trigger("click"));
	    }
    });
  });

  $('.footer-right a').click(function() {
  	$('.navbar-nav li').removeClass();
  	var clicked = $(this).attr("direct_to");
  	$('.navbar-nav li a').each(function(index) {
  		if($(this).attr('data-value')==clicked){
  			handleOutboundLinkClicks($(this).trigger("click"));
  		}
  	});
  });
  
  $('.banner a').click(function() {
  	$('.navbar-nav li').removeClass();
  	var clicked = $(this).attr("direct_to");
  	$('.navbar-nav li a').each(function(index) {
  		if($(this).attr('data-value')==clicked){
  			handleOutboundLinkClicks($(this).trigger("click"));
  		}
  	});
  });
  
  $(".navbar-nav li a").each(function(index){
    $(this).on("click", function(event){
      handleOutboundLinkClicks(event);
    }); 
  });

});