var goog_map = null;

var liaison = {
  init: function() {
    if (navigator.geolocation) {
        navigator.geolocation.getCurrentPosition(
            liaison.init_success,
            liaison.init_error
        );
    }
  },
      
  init_success: function(position) {
      $('#status').toggle();
      var goog_pos = new google.maps.LatLng(
          position.coords.latitude,
          position.coords.longitude
      );
      goog_map = new google.maps.Map(
          document.getElementById('canvas'),
          { center: goog_pos,
            zoom: 15,
            mapTypeId: google.maps.MapTypeId.ROADMAP
          }
      );
  },
  //     var me_marker = new google.maps.Marker({
  //         position: goog_pos,
  //         map: goog_map,
  //         title: "I think you're here."
  //     });
  // },

  init_error: function(msg) {
    $('#status').innerHtml = typeof msg == 'string' ? msg : "failed";
    $('#status').className = 'fail';
  },


  make_marker: function(lat,lon,title) {
      mk = new google.maps.Marker({ position: new google.maps.LatLng(lat, lon),
                               map: goog_map,
                               animation: google.maps.Animation.DROP,
                               title: title});
      google.maps.event.addListener(mk,'click',liaison.toggleBounce);
      goog_map_markers.push(mk);
  },

  toggle_bounce: function() {
      if (marker.getAnimation() != null) {
          marker.setAnimation(null);
      } else {
          marker.setAnimation(google.maps.Animation.BOUNCE);
      }
  },





  // beacon: function() {
  //     if (navigator.geolocation) {
  //         navigator.geolocation.getCurrentPosition(
  //             liaison.success,
  //             liaison.failure
  //         );
  //     }
  // },

  // success: function(pos) {
  //     $.ajax({
  //         type: "POST",
  //         url: "/beacon",
  //         data: { position: pos }});
  // },

  // failure: function(msg) {
  //     return false;
  // },
    
  loader: function() {
        $.getJSON('/gather',function(dat) {
            $.each(dat, function(k,v) {
                console.log("Lo: " + v.longitude);
                console.log("La: " + v.latitude);
                console.log("UID: " + v.uid);
                liaison.make_marker(v.latitude, v.longitude, v.uid, v.uid);
            });
        });
  }
};
