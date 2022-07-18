package com.multitimer.app;

import android.app.Service;
import android.content.Context;
import android.content.Intent;
import android.os.CountDownTimer;
import android.os.Handler;
import android.os.IBinder;
import android.os.Message;
import android.os.Messenger;

import androidx.annotation.Nullable;

import java.util.HashMap;
import java.util.Map;

public class TimerService extends Service {
    public static final int PAUSE_TIMER = 0;
    public static final int START_TIMER = 1;
    public static final int DELETE_TIMER = 2;

    private final Map<Integer, Timer> timers = new HashMap<>();
    private CountDownTimer countdown;

    class MsgHandler extends Handler {
        @Override
        public void handleMessage(Message msg) {
            switch(msg.what) {
                case START_TIMER:
                    Timer t = (Timer) msg.obj;
                    timers.put(t.getId(), t);
                    countdown = createTimer();
                    break;

                case DELETE_TIMER:
                    timers.remove(msg.arg1);
                    if(timers.isEmpty()){
                        countdown.cancel();
                        countdown = null;
                    }
            }
        }
    }

    private CountDownTimer createTimer() {
        if(timers.isEmpty()) {
            return null;
        }
        if(countdown != null) {
            return countdown;
        }
        CountDownTimer t = new CountDownTimer(Long.MAX_VALUE, 1000) {
            @Override
            public void onTick(long millisUntilFinished) {
                Context ctx = getApplicationContext();
                timers.entrySet().removeIf((entry) -> {
                   entry.getValue().tick(ctx);
                   return entry.getValue().isDone();
                });
            }

            @Override
            public void onFinish() {

            }
        };
        t.start();
        return t;
    }

    private final Messenger messenger = new Messenger(new MsgHandler());

    @Nullable
    @Override
    public IBinder onBind(Intent intent) {
        return messenger.getBinder();
    }
}