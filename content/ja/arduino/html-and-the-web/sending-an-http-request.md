---
date: 2024-01-20 17:59:11.863728-07:00
description: "How to: (\u65B9\u6CD5) ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.493155-06:00'
model: gpt-4-1106-preview
summary: .
title: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306E\u9001\u4FE1"
weight: 44
---

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
