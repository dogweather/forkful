---
title:                "ウェブページのダウンロード"
html_title:           "Arduino: ウェブページのダウンロード"
simple_title:         "ウェブページのダウンロード"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## なぜ
ウェブページをダウンロードする理由は様々です。例えば、外出先でスマートフォンを使用せずにコンピューターでウェブサイトを閲覧する必要がある場合や、特定のデータを取得したい場合などがあります。

## ダウンロードする方法 
以下のArduinoコードを使用して、特定のウェブサイトのコンテンツをダウンロードすることができます。

```Arduino
#include <WiFi.h>
#include <HTTPClient.h>

void setup() {
  Serial.begin(9600);

  WiFi.begin("Wi-Fiネットワークの名前", "パスワード");

  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.println("接続中...");
  }
}

void loop() {
  if (WiFi.status() == WL_CONNECTED) {
    HTTPClient http;

    http.begin("ウェブサイトのURL");
    int httpCode = http.GET();

    if (httpCode > 0) {
      String payload = http.getString();
      Serial.println(payload);
    }
    http.end();
  }
  delay(5000);
}
```

上記のコードは、Wi-Fiネットワークに接続し、指定したウェブサイトのコンテンツを取得した後、シリアルモニターに内容を出力します。ご自身の目的に合わせてコードをカスタマイズすることができます。

## 深堀り
HTTPClientライブラリを使用すると、より詳細な操作を行うことができます。例えば、POSTリクエストを使用してデータを送信したり、HTTPヘッダーを設定したりすることができます。また、セキュリティのためにHTTPSリクエストを行うこともできます。

## おまけ 
- [HTTPClientライブラリのドキュメント](https://arduino-esp8266.readthedocs.io/en/latest/esp8266wifi/httpclient-class.html)
- [ウェブサイトにリクエストを送信するプログラムのサンプルコード](https://randomnerdtutorials.com/esp32-http-get-post-arduino/)