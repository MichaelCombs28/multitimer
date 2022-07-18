package com.multitimer.app;

import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.ServiceConnection;
import android.os.Bundle;
import android.os.IBinder;
import android.os.Messenger;

import com.getcapacitor.BridgeActivity;

public class MainActivity extends BridgeActivity {
    private Messenger messenger;
    private boolean bound;
    private ServiceConnection connection = new ServiceConnection() {
        @Override
        public void onServiceConnected(ComponentName name, IBinder service) {
            messenger = new Messenger(service);
            bound = true;
        }

        @Override
        public void onServiceDisconnected(ComponentName name) {
            messenger = null;
            bound = false;
        }
    };

    @Override
    public void onStart() {
        super.onStart();
        NotificationUtils.createNotificationChannel(getApplicationContext());
        Intent intent = new Intent(this, TimerService.class);
        System.out.println("HERE");
        bindService(intent, connection, Context.BIND_AUTO_CREATE);
    }

    @Override
    public void onStop() {
        super.onStop();
        if(bound) {
            bound = false;
            unbindService(connection);
        }
    }

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        registerPlugin(BackgroundRunnerPlugin.class);
    }

    public Messenger getMessenger() {
        return messenger;
    }
}
