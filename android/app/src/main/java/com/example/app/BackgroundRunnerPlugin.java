package com.example.app;

import android.app.ActivityManager;
import android.app.AlarmManager;
import android.app.PendingIntent;
import android.content.Context;
import android.content.Intent;
import android.os.Message;
import android.os.Messenger;
import android.os.RemoteException;

import com.getcapacitor.Plugin;
import com.getcapacitor.PluginCall;
import com.getcapacitor.PluginMethod;
import com.getcapacitor.annotation.CapacitorPlugin;

@CapacitorPlugin(name = "BackgroundRunner")
public class BackgroundRunnerPlugin extends Plugin {
    public BackgroundRunnerPlugin() {
       super();
    }

    @PluginMethod()
    public void createTimer(PluginCall call) throws Exception {
        Message msg = Message.obtain(null, TimerService.START_TIMER, 0, 0, Timer.fromJsonObject((call.getData())));
        getMessenger().send(msg);
        call.resolve();
    }

    @PluginMethod()
    public void stopTimer(PluginCall call) throws Exception {
        Message msg = Message.obtain(null, TimerService.START_TIMER, call.getInt("id",0));
        getMessenger().send(msg);
        call.resolve();
    }

    private Messenger getMessenger() {
        MainActivity ma = (MainActivity)getActivity();
        return ma.getMessenger();
    }
}