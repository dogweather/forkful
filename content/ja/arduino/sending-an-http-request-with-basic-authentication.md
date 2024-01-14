---
title:                "Arduino: 基本認証を使用してhttpリクエストを送信する"
simple_title:         "基本認証を使用してhttpリクエストを送信する"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Why

Arduinoプログラミングを学ぶと、インターネットに接続して外部のウェブサイトなどから情報を取得し、その情報を使用してアクションを実行することができます。例えば、天気予報を取得してライトの色を変えるといったことができます。さらに、HTTPリクエストを使用することで、ウェブサイトなどからデータを取得するだけではなく、データを送信することもできるようになります。そして、今回はそのHTTPリクエストを送信する際に必要となる基本認証について説明します。

## How To

基本認証を使用してHTTPリクエストを送信するためには、まずはArduinoボードをインターネットに接続する必要があります。その後、以下のようなコードを使用してリクエストを作成します。

```Arduino
#include <WiFiClientSecure.h>  //必要なライブラリをインポート

char ssid[] = "WiFiネットワーク名";  //WiFiネットワーク名の入力
char pass[] = "WiFiのパスワード";   //WiFiのパスワードの入力
char server[] = "ウェブサイトのURL";  //リクエストを送信するウェブサイトのURLを入力

WiFiClientSecure client;  // WiFiClientSecureオブジェクトを作成

void setup() {
  Serial.begin(9600); //シリアルモニターの開始
  WiFi.begin(ssid, pass);  //WiFiに接続
  Serial.print("接続中"); 
  while (WiFi.status() != WL_CONNECTED) {  //WiFiに接続するまで待機
    Serial.print(".");
    delay(500);
  }
  Serial.println(" 接続完了");

  Serial.print("URL送信中... ");
  if (client.connect(server, 443)) {  //HTTPSポート443に接続
    Serial.println("成功!");
  } else {
    Serial.println("失敗");
  }
  
  //データを送信するリクエストを作成
  //必要に応じてユーザー名とパスワードを入力
  String request = "GET / HTTP/1.1\r\nAuthorization: Basic dXNlcjpwYXNz\r\nHost: ウェブサイトのURL\r\nConnection: close\r\n\r\n";
  client.println(request);  //リクエストを送信
}

void loop() {
  if (client.available()) {  //データが受信可能かどうかチェック
    while (client.available()) {  //すべてのデータを読み取るまで繰り返し処理
      char c = client.read();  //データを読み取り
      Serial.write(c);  //シリアルモニターに出力
    }
  }
  if (!client.connected()) {  //接続が切れたら
    Serial.println();
    Serial.println("データを送信完了");
    client.stop();  //接続を閉じる
    while (true);  //無限ループに入り、プログラムが停止する
  }
}
```

コード内の"Authorization: Basic dXNlcjpwYXNz"の部分は、ユーザー名とパスワードをBase64でエンコードしたものに置き換える必要があります。これにより、サーバーにログインするための認証情報をリクエストに含めることができます。

上記のコードを実行すると、ウェブサイトのHTMLコードがシリアルモニターに表示されるはずです。これで、基本認証を使用