const config = {
  appId: "com.example.app",
  appName: "multitmer",
  webDir: "build",
  bundledWebRuntime: false,
  ...(process.env.DEBUG === "true" && {
    server: {
      url: "http://10.0.0.88:9000",
      cleartext: true,
    },
  }),
};

module.exports = config;
