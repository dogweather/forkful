---
title:                "HTTPリクエストの送信"
aliases:
- /ja/arduino/sending-an-http-request/
date:                  2024-01-20T17:59:11.863728-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTPリクエストの送信"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/sending-an-http-request.md"
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
