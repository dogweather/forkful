---
title:                "ウェブページのダウンロード"
aliases:
- /ja/arduino/downloading-a-web-page.md
date:                  2024-01-20T17:43:43.384524-07:00
model:                 gpt-4-1106-preview
simple_title:         "ウェブページのダウンロード"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/downloading-a-web-page.md"
---

{{< edit_this_page >}}

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
