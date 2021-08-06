package com.example.app;

import android.content.Intent;

import com.getcapacitor.JSObject;

public class Timer {
    public enum TimerState {
        Counting,
        Paused,
        Ringing,
        RestCounting,
        RestPaused
    }

    private Integer id;
    private String label;
    private TimerState state;
    private Long startTime;
    private Long currentTime;
    private String tone;
    private Boolean vibrate;
    private Integer repetitions;
    private Integer currentRepetition;
    private Long restTime;
    private Long currentRestTime;
    private Long ringingTime;

    public static Timer fromJsonObject(JSObject o) throws Exception {
        return new Timer(
            TimerState.valueOf(o.getString("state")),
            o.getString("label"),
            o.getLong("startTime"),
            o.getLong("currentTime"),
            o.getString("tone"),
            o.getBoolean("vibrate"),
            o.getInt("repetitions"),
            o.getInt("currentRepetition"),
            o.getLong("restTime"),
            o.getLong("currentRestTime"),
            o.getInt("id")
        );
    }

    public static Timer fromIntent(Intent i) {
        return new Timer(
                TimerState.valueOf(i.getStringExtra("state")),
                i.getStringExtra("label"),
                i.getLongExtra("startTime", 0L),
                i.getLongExtra("currentTime", 0L),
                i.getStringExtra("tone"),
                i.getBooleanExtra("vibrate", false),
                i.getIntExtra("repetitions", 1),
                i.getIntExtra("currentRepetition", 1),
                i.getLongExtra("restTime", 0L),
                i.getLongExtra("currentRestTime", 0L),
                i.getIntExtra("id", 0)
        );
    }

    public void writeToIntent(Intent i) {
        i.putExtra("label", label);
        i.putExtra("startTime", startTime);
        i.putExtra("currentTime", currentTime);
        i.putExtra("tone", tone);
        i.putExtra("vibrate", vibrate);
        i.putExtra("repetitions", repetitions);
        i.putExtra("currentRepetition", currentRepetition);
        i.putExtra("restTime", restTime);
        i.putExtra("currentRestTime", currentRestTime);
        i.putExtra("state", state.toString());
        i.putExtra("id", id);
    }

    private Timer(TimerState state, String label, Long startTime, Long currentTime, String tone, Boolean vibrate, Integer repetitions, Integer currentRepetition, Long restTime, Long currentRestTime, Integer id) {
        this.state = state;
        this.label = label;
        this.startTime = startTime;
        this.currentTime = currentTime;
        this.tone = tone;
        this.vibrate = vibrate;
        this.repetitions = repetitions;
        this.currentRepetition = currentRepetition;
        this.restTime = restTime;
        this.currentRestTime = currentRestTime;
        this.id = id;
        this.ringingTime = 2000L;
    }

    public long getNextAlarmTime() {
        long t = state == TimerState.RestCounting ? currentRestTime : currentTime;
        return System.currentTimeMillis() + t;
    }

    public TimerState getState() {
        return state;
    }

    public String getLabel() {
        return label;
    }

    public Long getStartTime() {
        return startTime;
    }

    public Long getCurrentTime() {
        return currentTime;
    }

    public String getTone() {
        return tone;
    }

    public Boolean getVibrate() {
        return vibrate;
    }

    public Integer getRepetitions() {
        return repetitions;
    }

    public Integer getCurrentRepetition() {
        return currentRepetition;
    }

    public boolean alarmReceived() {
        // TODO do not reset if pause is available via intent
        // Reset timers
        if(currentRepetition < repetitions) {
            currentTime = startTime;
            currentRestTime = restTime;
            if (state == TimerState.RestCounting) {
                currentRepetition++;
                state = TimerState.Counting;
            } else {
                // State is counting
                if (restTime > 0) {
                    state = TimerState.RestCounting;
                } else {
                    // Rest time was not defined
                    state = TimerState.Counting;
                    currentRepetition++;
                }
            }
            return false;
        }
        return true;
    }

    public Long getRestTime() {
        return restTime;
    }

    public Long getCurrentRestTime() {
        return currentRestTime;
    }

    public Integer getId() {
        return id;
    }

    public Long getRingingTime() {
        return ringingTime;
    }
}
