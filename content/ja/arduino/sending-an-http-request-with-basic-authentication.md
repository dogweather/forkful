---
title:                "送信httpリクエストと基本認証"
html_title:           "Arduino: 送信httpリクエストと基本認証"
simple_title:         "送信httpリクエストと基本認証"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## なぜ
HTTPリクエストを基本認証で送信する理由は、他のデバイスやウェブサービスとの通信を安全に行うためです。基本認証はAPIキーを使用するよりも簡単な認証方法です。

## 方法
まず、HTTPClientライブラリをArduino IDEにインストールします。次に、```#include <ESP8266HTTPClient.h>```を追加し、WiFiライブラリもインクルードしてください。その後、以下のコードを使用してHTTPリクエストを送信します。

```
WiFiClient client;
HTTPClient http;

// リクエストを設定する
http.begin(client, "https://example.com/");
http.setAuthorization("ユーザー名", "パスワード");

// リクエストを送信する
int httpCode = http.GET();

if (httpCode > 0) { // 成功した場合
  String response = http.getString(); // レスポンスを取得する
  Serial.println(httpCode); // ステータスコードを表示する
  Serial.println(response); // レスポンスを表示する
}

http.end(); // リクエストを終了する
```

### 出力
サーバーからの応答を取得し、シリアルモニターに表示することができます。

```
HTTP/1.1 200 OK
Welcome to example.com!
```

## ディープダイブ
基本認証では、ユーザー名とパスワードを使用してサイトにログインする認証方法です。これにより、サーバーとの通信が安全になり、第三者によるデータの改ざんや盗難を防止することができます。また、APIキーを使用する場合は、キーを変更する必要があるため、基本認証の方が管理が簡単とも言えます。

## 参考リンク
- [HTTPClientライブラリのインストール方法](https://github.com/esp8266/Arduino/blob/master/doc/reference.md#httpclient)
- [基本認証のより詳細な説明](https://developer.mozilla.org/ja/docs/Web/HTTP/Authentication#Basic_authentication_scheme)