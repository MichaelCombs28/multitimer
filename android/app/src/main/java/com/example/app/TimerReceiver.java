package com.example.app;

import android.app.ActivityManager;
import android.app.AlarmManager;
import android.app.PendingIntent;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;

public class TimerReceiver extends BroadcastReceiver {

    public TimerReceiver() {
        super();
    }

    @Override
    public void onReceive(Context context, Intent intent) {
        AlarmManager alarmManager = (AlarmManager) context.getSystemService(Context.ALARM_SERVICE);
        Timer t = Timer.fromIntent(intent);
        boolean isComplete = t.alarmReceived();
        if(isComplete){
            if(isBackground()) {
                NotificationUtils.createRing(context, t);
            }
        } else {
            // No more reps
            Intent newIntent = new Intent(context, TimerReceiver.class);
            t.writeToIntent(newIntent);
            if(isBackground()) {
                NotificationUtils.createRest(context, t);
            }

            PendingIntent pendingIntent = PendingIntent.getBroadcast(
                    context,
                    t.getId(),
                    newIntent,
                    PendingIntent.FLAG_ONE_SHOT);

            alarmManager.setExactAndAllowWhileIdle(
                    AlarmManager.RTC_WAKEUP,
                    t.getNextAlarmTime(),
                    pendingIntent
            );
        }

    }

    public boolean isBackground() {
        ActivityManager.RunningAppProcessInfo proc = new ActivityManager.RunningAppProcessInfo();
        ActivityManager.getMyMemoryState(proc);
        return proc.importance != ActivityManager.RunningAppProcessInfo.IMPORTANCE_FOREGROUND;
    }


}
