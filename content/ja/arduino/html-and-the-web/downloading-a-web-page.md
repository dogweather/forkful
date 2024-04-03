---
date: 2024-01-20 17:43:43.384524-07:00
description: "How to: (\u65B9\u6CD5) Arduino\u3067Web\u30DA\u30FC\u30B8\u3092\u30C0\
  \u30A6\u30F3\u30ED\u30FC\u30C9\u3059\u308B\u306B\u306F\u3001Ethernet\u304BWiFi\u30B7\
  \u30FC\u30EB\u30C9\u304C\u5FC5\u8981\u3060\u3002\u4EE5\u4E0B\u306E\u4F8B\u3067\u306F\
  \u3001WiFi\u3092\u4F7F\u3063\u3066\u7C21\u6F54\u306B\u5B9F\u73FE\u3057\u3066\u3044\
  \u308B\u3088\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.496043-06:00'
model: gpt-4-1106-preview
summary: "Arduino\u3067Web\u30DA\u30FC\u30B8\u3092\u30C0\u30A6\u30F3\u30ED\u30FC\u30C9\
  \u3059\u308B\u306B\u306F\u3001Ethernet\u304BWiFi\u30B7\u30FC\u30EB\u30C9\u304C\u5FC5\
  \u8981\u3060\u3002\u4EE5\u4E0B\u306E\u4F8B\u3067\u306F\u3001WiFi\u3092\u4F7F\u3063\
  \u3066\u7C21\u6F54\u306B\u5B9F\u73FE\u3057\u3066\u3044\u308B\u3088."
title: "\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u306E\u30C0\u30A6\u30F3\u30ED\u30FC\u30C9"
weight: 42
---

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
