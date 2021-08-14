package com.example.app;

import android.app.ActivityManager;
import android.app.Notification;
import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.content.Context;
import android.net.Uri;

import androidx.core.app.NotificationCompat;

public class NotificationUtils {
    public static final String CHANNEL_ID = "multitimer.channel.alarms";
    public static final String CHANNEL_NAME = "alarms";

    public static void createNotificationChannel(Context ctx) {
        NotificationManager nm = (NotificationManager) ctx
                .getSystemService(Context.NOTIFICATION_SERVICE);
        NotificationChannel nc = new NotificationChannel(
                NotificationUtils.CHANNEL_ID,
                NotificationUtils.CHANNEL_NAME,
                NotificationManager.IMPORTANCE_HIGH
        );
        nc.enableVibration(true);
        nc.enableLights(true);
        nc.setLockscreenVisibility(Notification.VISIBILITY_PUBLIC);
        nm.createNotificationChannel(nc);
    }

    public static void createRing(Context ctx, Timer t) {
        long[] VIBRATE_PATTERN = new long[]{500L,500L};
        StringBuilder sb = new StringBuilder();
        sb.append("Time has elapsed.");
        if(t.getRepetitions() > 1) {
            String r = Integer.toString(t.getRepetitions());
            sb.append("\nRepetitions: ");
            sb.append(r);
            sb.append("/");
            sb.append(r);
        }
        createNotification(
                ctx,
                t.getId(),
                t.getLabel(),
                sb.toString(),
                t.getVibrate() ? VIBRATE_PATTERN : null,
                t.getTone()
        );
    }

    public static void createRest(Context ctx, Timer t) {
        long[] VIBRATE_PATTERN = new long[]{500L,500L};
        StringBuilder sb = new StringBuilder();
        String titleSuffix = t.getState() == Timer.TimerState.RestCounting ? ": Resting" : ": Counting";
        if(t.getRepetitions() > 1) {
            sb.append("\nRepetitions: ");
            sb.append(Integer.toString(t.getCurrentRepetition()));
            sb.append("/");
            sb.append(Integer.toString(t.getRepetitions()));
        }
        createNotification(
                ctx,
                t.getId(),
                t.getLabel() + titleSuffix,
                sb.toString(),
                t.getVibrate() ? VIBRATE_PATTERN : null,
                null
        );
    }

    private static boolean isBackground() {
        ActivityManager.RunningAppProcessInfo proc = new ActivityManager.RunningAppProcessInfo();
        ActivityManager.getMyMemoryState(proc);
        return proc.importance != ActivityManager.RunningAppProcessInfo.IMPORTANCE_FOREGROUND;
    }

    private static void createNotification(Context ctx, int id, String title, String content, long[] vibratePattern, String tone) {
        if(!isBackground()){
            return;
        }
        NotificationCompat.Builder builder = new NotificationCompat.Builder(ctx, CHANNEL_ID)
                .setContentTitle(title)
                .setContentText(content)
                .setSmallIcon(R.drawable.ic_launcher_foreground);
        if(vibratePattern != null) {
            builder.setVibrate(vibratePattern);
        }
        if(tone != null && !tone.equals("none")) {
            builder.setSound(Uri.parse("uri://" + tone));
        }
        NotificationManager nm = (NotificationManager) ctx.getSystemService(Context.NOTIFICATION_SERVICE);
        nm.notify(id, builder.build());
    }

    private NotificationUtils() {}
}
