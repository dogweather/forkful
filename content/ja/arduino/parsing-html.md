---
title:                "HTMLのパース"
html_title:           "Arduino: HTMLのパース"
simple_title:         "HTMLのパース"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/parsing-html.md"
---

{{< edit_this_page >}}

## 何を & なぜ?
HTMLのパースとは何かを説明し、プログラマーがそれをする理由を2〜3文で説明します。

HTMLのパースとは、HTMLのテキストファイルから情報を抽出することを指します。プログラマーがそれを行う理由の1つは、Webスクレイピングやデータの収集など、Web上の情報を取得する必要がある場合です。

## どのように:
```Arduino
#include <Arduino.h>
#include <SPI.h>
#include <WiFiNINA.h>
#include <WiFiClient.h>
#include <WiFi.h>
#include "index.html"

void setup() {
  Serial.begin(9600);
  WiFi.begin(ssid, password);

  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
  }
  Serial.print("Connected to WiFi network!");

  printHTML();
}

void loop() {}

void printHTML() {
  WiFiClient client;

  // Connect to the server
  if (client.connect(server, port)) {
    // Send GET request
    client.println("GET / HTTP/1.1");
    client.println("Host: www.example.com");
    client.println("Connection: close");
    client.println();

    // Read response
    while (client.available()){
      char c = client.read();
      Serial.write(c);
    }
  }

  // Disconnect
  client.stop();
}
```

`printHTML()`関数では、ArduinoがWebサイト「www.example.com」に接続し、HTMLファイルを取得してシリアルモニターに出力します。

## 深堀り:
HTMLのパースには、さまざまな方法があります。代表的な方法には、正規表現を使用したパターンマッチングや、パーサーライブラリを使用する方法があります。

また、HTMLのパースを行うことで、Webスクレイピング以外にも、Webアプリケーションの自動テストやスクリーンスクレイピングなど、さまざまな用途に活用することができます。

具体的な実装については、ArduinoのWiFiライブラリを使用してHTTPリクエストを送信し、レスポンスを取得することでHTMLのパースを行います。

## 参考資料:
更に詳しい情報を入手したい場合は、以下のリンクを参考にしてください。

- [Arduino WiFiライブラリドキュメント](https://www.arduino.cc/en/Reference/WiFiClient)
- [正規表現を使用したHTMLのパースの実装例](https://www.w3schools.com/jsref/jsref_match.asp)
- [HTMLパーサーの比較](https://en.wikipedia.org/wiki/Comparison_of_HTML_parsers)