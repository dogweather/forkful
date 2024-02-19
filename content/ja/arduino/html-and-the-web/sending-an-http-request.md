---
aliases:
- /ja/arduino/sending-an-http-request/
date: 2024-01-20 17:59:11.863728-07:00
description: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u9001\u308B\u3068\u306F\u3001\
  \u30A4\u30F3\u30BF\u30FC\u30CD\u30C3\u30C8\u4E0A\u306E\u30B5\u30FC\u30D0\u30FC\u306B\
  \u60C5\u5831\u3092\u6C42\u3081\u308B\u304B\u3001\u30C7\u30FC\u30BF\u3092\u9001\u308B\
  \u3053\u3068\u3067\u3059\u3002\u3053\u306E\u6A5F\u80FD\u306F\u3001IoT\u30C7\u30D0\
  \u30A4\u30B9\u304C\u5916\u306E\u4E16\u754C\u3068\u901A\u4FE1\u3059\u308B\u305F\u3081\
  \u306B\u3088\u304F\u4F7F\u308F\u308C\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:55.150432
model: gpt-4-1106-preview
summary: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u9001\u308B\u3068\u306F\u3001\u30A4\
  \u30F3\u30BF\u30FC\u30CD\u30C3\u30C8\u4E0A\u306E\u30B5\u30FC\u30D0\u30FC\u306B\u60C5\
  \u5831\u3092\u6C42\u3081\u308B\u304B\u3001\u30C7\u30FC\u30BF\u3092\u9001\u308B\u3053\
  \u3068\u3067\u3059\u3002\u3053\u306E\u6A5F\u80FD\u306F\u3001IoT\u30C7\u30D0\u30A4\
  \u30B9\u304C\u5916\u306E\u4E16\u754C\u3068\u901A\u4FE1\u3059\u308B\u305F\u3081\u306B\
  \u3088\u304F\u4F7F\u308F\u308C\u307E\u3059\u3002"
title: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306E\u9001\u4FE1"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
HTTPリクエストを送るとは、インターネット上のサーバーに情報を求めるか、データを送ることです。この機能は、IoTデバイスが外の世界と通信するためによく使われます。

## How to: (方法)
```Arduino
#include <ESP8266WiFi.h>

const char* ssid = "yourSSID";
const char* password = "yourPASSWORD";
const char* host = "api.example.com";

void setup() {
  Serial.begin(115200);
  WiFi.begin(ssid, password);

  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.print(".");
  }
  Serial.println("WiFi connected");

  WiFiClient client;
  
  if (!client.connect(host, 80)) {
    Serial.println("Connection failed");
    return;
  }
  
  client.print(String("GET /data") + " HTTP/1.1\r\n" +
               "Host: " + host + "\r\n" + 
               "Connection: close\r\n\r\n");
  while(client.available()){
    String line = client.readStringUntil('\r');
    Serial.print(line);
  }
}

void loop() {
  // Nothing to do here
}
```
このコードは、WiFi経由でHTTP GETリクエストを送り、レスポンスを受け取ります。

## Deep Dive (掘り下げ)
Arduinoは本来、単純なマイクロコントローラー用の開発プラットフォームでした。しかし、より強力なESP8266のようなチップが登場し、簡単にインターネットに接続可能になりました。HTTPを使う他の方法には、HTTPClientライブラリの使用やHTTPS接続もありますが、シンプルな接続には上記の基本的なTCPクライアントで十分です。実装の詳細には、接続の安定性や受信データの処理方法が重要になってくるでしょう。

## See Also (関連情報)
- ESP8266 WiFi documentation: https://arduino-esp8266.readthedocs.io/en/latest/esp8266wifi/readme.html
- Arduino HTTP Client library: https://www.arduino.cc/en/Tutorial/LibraryExamples/HttpClient
- Handling HTTP on Arduino (tutorial): https://randomnerdtutorials.com/esp8266-http-get-post-arduino/
