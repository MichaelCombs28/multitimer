package com.example.app;

import android.os.Bundle;

import com.getcapacitor.BridgeActivity;

public class MainActivity extends BridgeActivity {
    @Override
    public void onStart() {
        super.onStart();
        NotificationUtils.createNotificationChannel(getApplicationContext());
    }

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        registerPlugin(BackgroundRunnerPlugin.class);
    }
}
