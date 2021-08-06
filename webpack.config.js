const webpack = require("webpack");
const path = require("path");
require("@capacitor/core");
const HtmlWebpackPlugin = require("html-webpack-plugin");

module.exports = {
  entry: "./src/index.js",
  mode: "development",
  output: {
    path: path.resolve(__dirname, "build"),
    filename: "[name][fullhash].js",
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
  plugins: [
    new HtmlWebpackPlugin({
      title: "Multitimer",
      inject: "head",
      meta: {
        viewport:
          "viewport-fit=cover, width=device-width, initial-scale=1.0, minimum-scale=1.0, maximum-scale=1.0, user-scalable=no",
        "mobile-web-app-capable": "yes",
        "apple-mobile-web-app-capable": "yes",
      },
    }),
  ],
  devServer: {
    contentBase: path.join(__dirname, "build"),
    compress: true,
    port: 9000,
    host: "0.0.0.0",
  },
};
