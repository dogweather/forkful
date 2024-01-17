---
title:                "「httpリクエストの送信」"
html_title:           "Arduino: 「httpリクエストの送信」"
simple_title:         "「httpリクエストの送信」"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 何 & なぜ？

HTTPリクエストを送信するとは、Arduinoがインターネット上の別のサーバーにデータを要求することを意味します。プログラマーがこれを行う理由は、Arduinoがインターネット上の情報を取得したり、オンラインサービスとやり取りしたりすることができるようにするためです。

## 方法：

```Arduino
#include <WiFiClient.h>
#include <ESP8266WiFi.h>

void setup() {

 //WiFiに接続するための情報を設定
 const char* ssid = "WiFiネットワーク名";
 const char* password = "WiFiパスワード";

 //WiFi接続を開始
 WiFi.begin(ssid, password);

 //WiFiが接続されるまで待機
 while (WiFi.status() != WL_CONNECTED) {
   delay(500);
   Serial.println("Connecting to WiFi..");
 }

 //HTTPリクエストを送信するためのクライアントを作成
 WiFiClient client;

 //リクエストを送信する先のサーバーを指定
 if (client.connect("example.com", 80)) {

   //リクエストを文字列で作成
   client.print("GET / HTTP/1.1\r\n");
   client.print("Host: example.com\r\n");
   client.print("Connection: close\r\n\r\n");

   //サーバーからのレスポンスを読み込み
   while (client.available()) {
     String line = client.readStringUntil('\r');
     Serial.print(line);
   }

   //接続を閉じる
   client.stop();
 }
}

void loop() {
 //何もしない
}
```

実行すると、シリアルモニターにサーバーのレスポンスが表示されます。

## 深く掘り下げる：

1. HTTPリクエストは、ウェブブラウザーとサーバー間の通信を可能にするために開発されたプロトコルです。
2. この例では、[ESP8266WiFiライブラリ](https://arduino-esp8266.readthedocs.io/en/latest/)を使用してHTTPリクエストを送信していますが、[Ethernetライブラリ](https://www.arduino.cc/en/Reference/Ethernet)を使用することもできます。
3. HTTPリクエストは、GETメソッドの他にPOST、PUT、DELETEなどのメソッドで送信することもできます。詳細については[こちら](https://developer.mozilla.org/ja/docs/Web/HTTP/Methods)を参照してください。

## 参考：

- [HTTPリクエストについてのArduino公式ドキュメント](https://www.arduino.cc/en/Tutorial/HttpClient)
- [HTTPメソッドについてのMDNウェブドキュメント](https://developer.mozilla.org/ja/docs/Web/HTTP/Methods)