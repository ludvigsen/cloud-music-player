Elm.Native.GoogleAPI = {};
Elm.Native.GoogleAPI.make = function(localRuntime){
  localRuntime.Native = localRuntime.Native || {};
  localRuntime.Native.GoogleAPI = localRuntime.Native.GoogleAPI || {};
  
  var SCOPES, CLIENT_ID;
  /**
   * Called when authorization server replies.
   *
   * @param {Object} authResult Authorization result.
   */
  function handleAuthResult(authResult) {

    var authButton = document.createElement('input');
    authButton.type = "button";
    authButton.id = "authorizeButton";
    authButton.value = "Authorize";
    document.querySelector('body').appendChild(authButton);
    //var filePicker = document.getElementById('filePicker');
    authButton.style.display = 'none';
    //filePicker.style.display = 'none';
    if (authResult && !authResult.error) {
      // Access token has been successfully retrieved, requests can be sent to the API.
      //filePicker.style.display = 'block';
      //filePicker.onchange = uploadFile;
      console.log('authResult: ', authResult);
      console.log('SCOPES: ', SCOPES, 'CLIENT_ID', CLIENT_ID);
    } else {
      // No access token could be retrieved, show the button to start the authorization flow.
      authButton.style.display = 'block';
      authButton.onclick = function() {
        gapi.auth.authorize(
          {'client_id': CLIENT_ID, 'scope': SCOPES, 'immediate': false},
          handleAuthResult);
      };
    }
  }

  localRuntime.Native.GoogleAPI.values = {
    checkAuth: function(clientId, scopes) {
      console.log('checkAuth: ', clientId, scopes);
      SCOPES=scopes;CLIENT_ID=clientId;
      gapi.auth.authorize(
        {'client_id': clientId, 'scope': scopes, 'immediate': true},
        handleAuthResult);
    }
  };
  return localRuntime.Native.GoogleAPI.values;
};
