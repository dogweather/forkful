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

## なぜ

HTTPリクエストを送信することに関わる動機は、インターネット上の情報にアクセスしたいと思うことから来ています。例えば、ウェブサイトやAPIからデータを取得したい、クラウドサービスにデータを送信したいといった場合に、HTTPリクエストが必要になってきます。

## 作り方

Arduinoには、EthernetライブラリというHTTPリクエストを送信するための機能があります。以下のように、ライブラリを使用してGETリクエストを送信する例を示します。

```Arduino
#include <SPI.h>
#include <Ethernet.h>

// イーサーネットシールドを使用している場合、ピン番号を指定してイーサーネットオブジェクトを作成
// Ethernetクラス の引数にはMACアドレスを指定
EthernetClient client;
char server[] = "www.example.com";

void setup() {
  // シリアルポートを開き、コンソールにメッセージを表示
  Serial.begin(9600);
  Serial.println("HTTPリクエストを送信します。");

  // イーサーネット初期化
  Ethernet.begin(mac);
  // IPアドレスをシリアルモニターに表示
  Serial.print("IP アドレス：");
  Serial.println(Ethernet.localIP());
}

void loop() {
  // HTTPリクエストの要求を開始
  if (client.connect(server, 80)) {
    // サーバーにGETリクエストを送信
    client.println("GET / HTTP/1.1");
    client.println("Host: www.example.com");
    client.println("Connection: close");
    client.println();
  }
  // サーバーからのレスポンスを受信
  while (client.available()) {
    // シリアルモニターにレスポンスを表示
    char c = client.read();
    Serial.print(c);
  }
  // サーバーとの接続を閉じる
  client.stop();
}

```

実行すると、シリアルモニターにサーバーからのレスポンスが表示されます。また、EthernetライブラリにはHTTPリクエストを送信するための関数が他にもありますので、詳細は公式ドキュメントを参照してください。

## 深堀り

HTTPリクエストは、トランスポート層のTCPプロトコルを使用して通信を行います。TCPプロトコルによってデータが断片化されることなく、確実に送信および受信されることが保証されます。また、リクエストにはヘッダーとボディの2つの部分があり、ヘッダーにはリクエストの種類や受け取り元・宛先の情報が含まれ、ボディにはリクエストに関連するデータが含まれます。HTTPリクエストを送信する際は、これらの情報を適切に設定することが重要です。

## 参考リンク

- [Arduino Ethernet Library Reference](https://www.arduino.cc/en/Reference/Ethernet)
- [TCP Protocol](https://www.tutorialspoint.com/transport_layer/tcp_protocol.htm)
- [HTTP Headers Reference](https://www.tutorialspoint.com/http/http_headers.htm)