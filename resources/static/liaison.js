
var liaison = {
  success: function(position) {
      $('#status').toggle();
      var goog_pos = new google.maps.LatLng(
          position.coords.latitude,
          position.coords.longitude);
      var goog_map = new google.maps.Map(
          document.getElementById('canvas'),
          { center: goog_pos,
            zoom: 15,
            mapTypeId: google.maps.MapTypeId.ROADMAP
          });
      var me_marker = new google.maps.Marker({
          position: goog_pos,
          map: goog_map,
          title: "I think you're here."
      });
  },
  error: function(msg) {
    $('#status').innerHtml = typeof msg == 'string' ? msg : "failed";
    $('#status').className = 'fail';
  },
  mapfire: function() {
    if (navigator.geolocation) {
        navigator.geolocation.getCurrentPosition(liaison.success,liaison.error);
    }
  },
  beacon: function() {
      if (navigator.geolocation) {
          navigator.geolocation.getCurrentPosition(liaison.beacon_success, liaison.beacon_failure);
      }
  },
  beacon_success: function(pos) {
      $.ajax({
          type: "POST",
          url: "/beacon",
          data: { position: pos }});
  },
  beacon_failure: function(msg) {
      return false;
  }
}


