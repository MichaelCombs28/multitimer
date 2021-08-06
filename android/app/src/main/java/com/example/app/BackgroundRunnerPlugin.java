package com.example.app;

import android.app.AlarmManager;
import android.app.PendingIntent;
import android.content.Context;
import android.content.Intent;

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
    public void startBackground(PluginCall call) throws Exception {
        System.out.println(call.getData().toString());
    }

    @PluginMethod()
    public void createAlarm(PluginCall call) throws Exception {
        AlarmManager alarmManager = (AlarmManager) getContext().getSystemService(Context.ALARM_SERVICE);
        Timer t = Timer.fromJsonObject(call.getData());
        System.out.println(call.getData());
        if(t.getState() == Timer.TimerState.Counting) {
            if(t.getCurrentTime() <= 1000L) {
                if(t.alarmReceived()) {
                    call.resolve();
                    return;
                }
            }
        } else {
            if(t.getCurrentRestTime() <= 1000L) {
                t.alarmReceived();
            }
        }
        Intent intent = new Intent(getContext(), TimerReceiver.class);
        t.writeToIntent(intent);

        PendingIntent pendingIntent = PendingIntent.getBroadcast(
                getContext(),
                t.getId(),
                intent,
                PendingIntent.FLAG_ONE_SHOT);
        alarmManager.setExactAndAllowWhileIdle(AlarmManager.RTC_WAKEUP,
                t.getNextAlarmTime(),
                pendingIntent);
        call.resolve();
    }

    @PluginMethod()
    public void stopAlarm(PluginCall call) throws Exception {
        AlarmManager alarmManager = (AlarmManager) getContext().getSystemService(Context.ALARM_SERVICE);
        Intent intent = new Intent(getContext(), TimerReceiver.class);
        //intent.putExtra("state", "Paused");
        int id = call.getInt("id");
        PendingIntent pendingIntent = PendingIntent.getBroadcast(
                getContext(), id, intent, PendingIntent.FLAG_ONE_SHOT);
        alarmManager.cancel(pendingIntent);
        call.resolve();
    }
}