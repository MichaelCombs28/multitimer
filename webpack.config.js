const webpack = require("webpack");
const path = require("path");
require("@capacitor/core");

module.exports = {
  entry: "./src/index.js",
  mode: "development",
  output: {
    path: path.resolve(__dirname, "build"),
    filename: "./elm.js",
    assetModuleFilename: "[name][ext][query]",
  },
  module: {
    rules: [
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        use: {
          loader: "elm-webpack-loader",
          options: {
            debug: process.env.DEBUG,
          },
        },
      },
      {
        test: /\.(mp3|ttf|woff|woff2|svg|eot|png)$/,
        type: "asset/resource",
      },
      {
        test: /\.css$/i,
        use: ["style-loader", "css-loader"],
      },
      {
        test: /\.html$/,
        use: "html-loader",
      },
    ],
  },
  plugins: [],
  devServer: {
    contentBase: path.join(__dirname, "build"),
    compress: true,
    port: 9000,
    host: "0.0.0.0",
  },
};
