---
date: 2024-01-20 17:43:43.384524-07:00
description: "Web\u30DA\u30FC\u30B8\u3092\u30C0\u30A6\u30F3\u30ED\u30FC\u30C9\u3059\
  \u308B\u3063\u3066\uFF1FWeb\u4E0A\u306E\u60C5\u5831\u3092\u624B\u5143\u306E\u30C7\
  \u30D0\u30A4\u30B9\u306B\u53D6\u308A\u8FBC\u3080\u3053\u3068\u3055\u3002\u306A\u305C\
  \u3084\u308B\u306E\u304B\uFF1F\u30C7\u30FC\u30BF\u3092\u5206\u6790\u3057\u305F\u308A\
  \u3001\u30EA\u30A2\u30EB\u30BF\u30A4\u30E0\u306E\u60C5\u5831\u3092\u6D3B\u7528\u3059\
  \u308B\u305F\u3081\u3060\u3088\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.496043-06:00'
model: gpt-4-1106-preview
summary: "Web\u30DA\u30FC\u30B8\u3092\u30C0\u30A6\u30F3\u30ED\u30FC\u30C9\u3059\u308B\
  \u3063\u3066\uFF1FWeb\u4E0A\u306E\u60C5\u5831\u3092\u624B\u5143\u306E\u30C7\u30D0\
  \u30A4\u30B9\u306B\u53D6\u308A\u8FBC\u3080\u3053\u3068\u3055\u3002\u306A\u305C\u3084\
  \u308B\u306E\u304B\uFF1F\u30C7\u30FC\u30BF\u3092\u5206\u6790\u3057\u305F\u308A\u3001\
  \u30EA\u30A2\u30EB\u30BF\u30A4\u30E0\u306E\u60C5\u5831\u3092\u6D3B\u7528\u3059\u308B\
  \u305F\u3081\u3060\u3088\u3002."
title: "\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u306E\u30C0\u30A6\u30F3\u30ED\u30FC\u30C9"
weight: 42
---

## What & Why? (何となぜ？)
Webページをダウンロードするって？Web上の情報を手元のデバイスに取り込むことさ。なぜやるのか？データを分析したり、リアルタイムの情報を活用するためだよ。

## How to: (方法)
ArduinoでWebページをダウンロードするには、EthernetかWiFiシールドが必要だ。以下の例では、WiFiを使って簡潔に実現しているよ。

```Arduino
#include <WiFi.h>

const char* ssid     = "yourNetworkName";  // WiFiネットワーク名
const char* password = "yourNetworkPass";  // WiFiパスワード
const char* host     = "example.com";      // ダウンロードするサイトのドメイン

void setup() {
  Serial.begin(115200);
  WiFi.begin(ssid, password);
  
  while (WiFi.status() != WL_CONNECTED) {
    delay(1000);
    Serial.println("Connecting to WiFi...");
  }

  Serial.println("Connected to WiFi");
  
  WiFiClient client;
  if (!client.connect(host, 80)) {
    Serial.println("Connection failed");
    return;
  }
  
  String url = "/";  // ダウンロードするページのパス
  client.println("GET " + url + " HTTP/1.1");
  client.println("Host: " + String(host));
  client.println("Connection: close");
  client.println();
}

void loop() {
  delay(10000);  // 10秒おきに実行
  
  WiFiClient client;
  if (client.connect(host, 80)) {
    client.println("GET / HTTP/1.1");
    client.println("Host: " + String(host));
    client.println("Connection: close");
    client.println();
    
    while (client.connected()) {
      String line = client.readStringUntil('\n');
      if (line == "\r") {
        break;
      }
    }

    // Webページの中身を読み取る
    String line = client.readStringUntil('\n');
    Serial.println(line);
    Serial.println();
  }
}
```

## Deep Dive (深掘り)
過去、Webページのダウンロードは主にPC上で行われた。しかし、IoTの発展により、Arduinoのようなマイコンでもよく使われるようになったね。Ethernetシールドを使う方法もあるけど、WiFiの方が手軽だよ。実装にはHTTPプロトコルを利用して、GETリクエストをWebサーバに送り、応答を受け取る。落としたページをどう使うかはプログラマの創造性にかかっている。

## See Also (関連情報)
- Arduino公式サイトのWiFiライブラリについてのドキュメント: [https://www.arduino.cc/en/Reference/WiFi](https://www.arduino.cc/en/Reference/WiFi)
- HTTPリクエストの基礎: [https://www.w3schools.com/tags/ref_httpmethods.asp](https://www.w3schools.com/tags/ref_httpmethods.asp)
- Arduinoを使ったIoTプロジェクト例: [https://create.arduino.cc/projecthub](https://create.arduino.cc/projecthub)
