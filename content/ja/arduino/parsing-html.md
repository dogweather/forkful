---
title:                "「HTMLの解析」"
html_title:           "Arduino: 「HTMLの解析」"
simple_title:         "「HTMLの解析」"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/parsing-html.md"
---

{{< edit_this_page >}}

## なぜHTMLのパースを行うのか

HTMLは、今日のインターネット世界で欠かせないものです。ウェブページの文章や画像、ビデオなどは、ほとんどがHTMLで作られています。よって、Arduinoプログラマーにとっても、HTMLをパース（解析）することは重要なスキルです。

## パーシングの実装方法

```Arduino
#include <ESP8266WiFi.h> // ESP8266のWi-Fiライブラリをインクルード

char ssid[] = "YOUR_WIFI_SSID"; // Wi-FiのSSIDを指定
char password[] = "YOUR_WIFI_PASSWORD"; // Wi-Fiのパスワードを指定

void setup() {
  Serial.begin(115200); //シリアル通信の速度を設定
  WiFi.begin(ssid, password); //Wi-Fiに接続
  while (WiFi.status() != WL_CONNECTED) { //Wi-Fi接続が完了するまでループ
    delay(500);
    Serial.print(".");
  }
  Serial.println("Connected to Wi-Fi!"); //Wi-Fi接続が完了したらシリアルモニターにメッセージを表示

  WiFiClient client; //クライアントを作成
  const char* host = "www.example.com"; //HTMLをパースするサイトのURLを指定
  if (!client.connect(host, 80)) { //クライアントがホストに接続できなかった場合はエラーを表示
    Serial.println("Connection failed.");
    return;
  }
  
  client.print("GET /page.html HTTP/1.1\r\n"); //GETリクエストを送信
  client.print("Host: ");
  client.print(host);
  client.print("\r\n");
  client.print("Connection: close\r\n\r\n"); //ヘッダーの終わりを示すために空行を送信

  while(client.connected()) { //クライアント接続が確立している間ループ
    String line = client.readStringUntil('\r'); //HTMLの行を読み込む
    if (line == "\n") { //空行が来たらループを抜ける
      break;
    }
    Serial.print(line); //HTMLの行をシリアルに表示
  }
}

void loop() {

}

```

## パーシングの詳細

HTMLのパースは、HTMLドキュメントの特定の部分を取得することを意味します。Arduinoでは、HTMLをテキストとして読み込んで、特定のパターンやキーワードを検出することで、必要な情報を取得することができます。しかし、HTMLの構造やタグの種類を理解することが重要です。よって、HTMLのパースを行う際は、HTMLの基本的な知識を身につけることが大切です。

## 関連リンク

- [Arduinoプログラミングチュートリアル - HTMLのパース](https://www.arduino.cc/en/Tutorial/LibraryExamples/UrlClient)
- [HTMLの基礎](https://developer.mozilla.org/ja/docs/Web/HTML)
- [ESP8266WiFiライブラリのドキュメント](https://arduino-esp8266.readthedocs.io/en/latest/esp8266wifi/client-examples.html)