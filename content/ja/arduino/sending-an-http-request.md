---
title:                "Arduino: 「httpリクエストの送信」"
simple_title:         "「httpリクエストの送信」"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/sending-an-http-request.md"
---

{{< edit_this_page >}}

## なぜ

Arduinoのプログラミングにおいて、HTTPリクエストを送信することはとても重要な役割を果たします。例えば、センサーから収集したデータをクラウド上のサーバーにアップロードすることで、リアルタイムでデータを収集・管理・解析することが可能となります。

## 方法

HTTPリクエストを送信するには、まずはWi-FiシールドやEthernetシールドなどのインターネット接続用のモジュールをArduinoに接続し、ネットワークに接続する必要があります。

次に、`HttpClient.h`ライブラリをインポートし、リクエストを送信するURLを指定します。例えば、以下のようなコードでリクエストを送信することができます。

```Arduino
#include <SPI.h>
#include <Ethernet.h>
#include <HttpClient.h>

byte mac[] = {0xDE, 0xAD, 0xBE, 0xEF, 0xFE, 0xED}; // ArduinoのMACアドレス
IPAddress server(192,168,1,1); // リクエストを送信するURLのIPアドレス
EthernetClient client;

void setup() {
  Ethernet.begin(mac); // イーサネット接続を開始
  Serial.begin(9600); // シリアル通信を開始
}

void loop() {
  if (client.connect(server, 80)) { // 指定したIPアドレスのポート80に接続する
    client.println("GET / HTTP/1.1"); // リクエストの種類を指定
    client.println("Host: 192.168.1.1"); // リクエストを送信するホストを指定
    client.println("Connection: close"); // コネクションをクローズすることを指定
    client.println(); // 改行
  }

  while (client.connected()) {
    if (client.available()) { // サーバーからのレスポンスを読み取る
      char c = client.read();
      Serial.print(c); // シリアルモニターにレスポンスを出力
    }
  }

  client.stop(); // リクエスト後にコネクションをクローズする
  delay(10000); // 10秒待機
}
```

リクエストを送信する際には、HTTPメソッドとしてGETやPOSTなどを指定することもできます。

## ディープダイブ

HTTPリクエストを送信する際には、ネットワークのセキュリティにも注意する必要があります。HTTPSプロトコルを使用することで、データのやり取りを暗号化することができます。さらに、サーバー側では認証のためのAPIキーなども必要となる場合がありますので、コード内に適切な認証情報を含めることも重要です。

## おすすめのリンク

- [HttpClientライブラリのドキュメント](https://github.com/amcewen/HttpClient)
- [ArduinoでのHTTPリクエストの方法](https://www.arduino.cc/en/Tutorial/HttpClient)
- [Wi-FiシールドとEthernetシールドの違い](https://www.electroschematics.com/differences-wifi-ethernet-shield/)
- [HTTPSプロトコルについての解説](https://www.cloudflare.com/ja-jp/learning/ssl/what-is-https/)
- [サーバー側でのAPI認証についての記事](https://www.smashingmagazine.com/2018/01/understanding-using-rest-api/)
- [Arduinoのオフィシャルフォーラム](https