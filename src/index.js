import { Haptics } from "@capacitor/haptics";
import { LocalNotifications } from "@capacitor/local-notifications";

import "onsenui/css/onsen-css-components.css";
import "onsenui/esm";

// Fonts
// Images
import "./ringing.png";

// Sounds
import "./beep.mp3";
import "./heel_walk.mp3";
import "./missle_alarm.mp3";
import "./normal.mp3";
import "./pager.mp3";
import "./sigh.mp3";
import "./temple_bell.mp3";

// Program
import queue from "async/queue";
import { Elm } from "./Main.elm";

const app = Elm.Main.init({
  node: document.getElementById("main"),
});

/*
LocalNotifications.addListener("localNotificationActionPerformed", ({ id }) => {
  try {
    app.ports.fromJS.send({ type: "NotificationClosed", id })
  } catch(e) {
    alert(e.message);
  }
});
*/

const alarms = {};
const vibrate = {};

const eventHandler = async ({ type, ...event }) => {
  switch (type) {
    case "ring": {
      if (event.vibrate) {
        Haptics.vibrate({ duration: 1000 })
        vibrate[event.id] = setInterval(
          () => Haptics.vibrate({ duration: 1000 }),
          2000
        );
      }

      if (event.tone !== "none") {
        const a = new Audio(event.tone);
        a.loop = true;
        alarms[event.id] = a;
        a.play();
        try {
          LocalNotifications.schedule({
            notifications: [
              {
                id: event.id,
                title: event.label,
                autoCancel: true,
                summaryText: "Time has elapsed!",
              },
            ],
          });
        } catch (e) {
          alert(e.message);
        }
      }
      return;
    }
    case "stopAlarm": {
      alarms[event.id]?.pause();
      delete alarms[event.id];

      if(vibrate[event.id]) {
        clearInterval(vibrate[event.id]);
        delete vibrate[event.id];
      }
      return;
    }
  }
};

// Event queue with a single worker run at a time
const eventQueue = queue(eventHandler);

app.ports.fromElm.subscribe((event) => eventQueue.push(event));
