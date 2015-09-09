Elm.Native.GoogleAPI = {};
Elm.Native.GoogleAPI.make = function(localRuntime){
  localRuntime.Native = localRuntime.Native || {};
  localRuntime.Native.GoogleAPI = localRuntime.Native.GoogleAPI || {};
  return localRuntime.Native.GoogleAPI.values = {
    checkAuth: function(clientId, scopes) {
      gapi.auth.authorize(
        {'client_id': clientId, 'scope': scopes, 'immediate': true},
        handleAuthResult);
    }
  };
};
