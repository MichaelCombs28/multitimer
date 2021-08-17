// Plugins
import { Device } from "@capacitor/device";
import { Haptics } from "@capacitor/haptics";
import { KeepAwake } from "@capacitor-community/keep-awake";
import { App } from "@capacitor/app";
import { Storage } from "@capacitor/storage";
import { registerPlugin } from "@capacitor/core";

import "onsenui/css/onsen-css-components.css";
import "onsenui/esm";

// Fonts
// Images
import "./img/ringing.png";
import "./img/splashCenter.png";

// Sounds
import "./mp3/beep.mp3";
import "./mp3/heel_walk.mp3";
import "./mp3/missle_alarm.mp3";
import "./mp3/normal.mp3";
import "./mp3/pager.mp3";
import "./mp3/sigh.mp3";
import "./mp3/temple_bell.mp3";

// Program
import queue from "async/queue";
import { Elm } from "./Main.elm";

const BackgroundRunner = registerPlugin("BackgroundRunner");
const alarms = {};
const vibrate = {};

(async () => {
  const device = await Device.getInfo();
  const { value: flags } = await Storage.get({ key: "flags" });
  const app = Elm.Main.init({
    node: document.getElementsByTagName("body")[0],
    flags: flags ? { ...device, ...JSON.parse(flags) } : null,
  });
  const eventHandler = async ({ type, ...event }) => {
    switch (type) {
      case "ring": {
        if (event.vibrate) {
          Haptics.vibrate({ duration: event.duration });
          vibrate[event.id] = setInterval(
            () => Haptics.vibrate({ duration: event.duration }),
            event.interval
          );
        }

        if (event.tone !== "none") {
          const a = new Audio(event.tone);
          a.loop = true;
          alarms[event.id] = a;
          a.play();
        }
        return;
      }
      case "stopRing": {
        alarms[event.id]?.pause();
        delete alarms[event.id];

        if (vibrate[event.id]) {
          clearInterval(vibrate[event.id]);
          delete vibrate[event.id];
        }
        return;
      }
      case "hapticVibrate": {
        Haptics.vibrate({ duration: event.duration });
        return;
      }
      case "keepAwake": {
        await KeepAwake.keepAwake();
        return;
      }
      case "allowSleep": {
        await KeepAwake.allowSleep();
        return;
      }
      case "timers": {
        console.log("Sending timers to background task");
        //await BackgroundRunner.startBackground({ timers: event.timers });
        return;
      }
      case "nowActive": {
        /*
        const { timers } = await BackgroundRunner.stopBackground();
        app.ports.fromJS.send({
          type: "BackgroundTimerUpdates",
          timers,
        });
        */
        return;
      }
      case "storage": {
        if (event.value === null) {
          await Stroage.remove({ key: event.key });
        } else {
          await Storage.set({
            key: event.key,
            value: JSON.stringify(event.value),
          });
        }
        return;
      }
      case "alarmStarted": {
        delete event.type;
        await BackgroundRunner.createTimer({
          ...event,
        });
        return;
      }
      case "alarmStopped": {
        await BackgroundRunner.stopTimer({ id: event.id });
        return;
      }
    }
  };

  // Event queue with a single worker run at a time
  const eventQueue = queue(eventHandler);
  app.ports.fromElm.subscribe((event) => eventQueue.push(event));

  // Lock screen functionality
  App.addListener("appStateChange", ({ isActive }) => {
    app.ports.fromJS.send({
      type: "AppStateChanged",
      isActive,
    });
  });
})();
