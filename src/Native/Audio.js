Elm.Native.Audio = {};
Elm.Native.Audio.make = function(localRuntime){
  localRuntime.Native = localRuntime.Native || {};
  localRuntime.Native.Audio = localRuntime.Native.Audio || {};
  return localRuntime.Native.Audio.values = {
    play: function(selector){
        setTimeout(function(){
          try{
            var player = document.querySelector(selector);
            player.load();
            player.play();
          }catch(e){ }
        }, 1500);
      return "";
    }
  };
};
