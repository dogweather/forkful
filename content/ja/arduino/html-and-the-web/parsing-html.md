---
title:                "HTMLの解析"
aliases:
- /ja/arduino/parsing-html/
date:                  2024-02-03T19:11:54.371913-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTMLの解析"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？

ArduinoプロジェクトでのHTMLの解析とは、ウェブページから情報を抽出することです。プログラマーは、Arduinoデバイスがインターネットと対話できるようにするため、ホームオートメーションから環境モニタリングまでの目的でウェブサイトからデータを収集するためにこれを行います。

## 方法:

ArduinoでのHTMLの解析には、通常、限られたデバイスリソースのために最小限のフットプリントライブラリが必要です。ウェブスクレイピングと解析に人気な選択肢は、Wi-Fi機能とHTTPプロトコルのネイティブサポートを考慮して、ESP8266用の`ESP8266HTTPClient`と`ESP8266WiFi`ライブラリ、あるいはそれらのESP32バージョンを使用することです。ここでは、ESP8266やESP32を使用していると仮定して、HTMLを取得し解析する基本例を示します：

まず、必要なライブラリを含む：
```cpp
#include <ESP8266WiFi.h> // ESP8266用
#include <ESP8266HTTPClient.h>
#include <WiFiClient.h>
// ESP32を使用している場合は、同等のESP32ライブラリを使用してください

const char* ssid = "yourSSID";
const char* password = "yourPASSWORD";
```

Wi-Fiネットワークに接続する：
```cpp
void setup() {
    Serial.begin(115200);
    WiFi.begin(ssid, password);

    while (WiFi.status() != WL_CONNECTED) {
        delay(1000);
        Serial.println("接続中...");
    }
}
```

HTTPリクエストを行い、簡単なHTMLピースを解析する：
```cpp
void loop() {
    if (WiFi.status() == WL_CONNECTED) { //WiFi接続状況をチェック
        HTTPClient http;  //HTTPClientクラスのオブジェクトを宣言

        http.begin("http://example.com");  //リクエストの宛先を指定
        int httpCode = http.GET();  //リクエストを送信

        if (httpCode > 0) { //戻ってきたコードのチェック
            String payload = http.getString();   //リクエストレスポンスペイロードを取得
            Serial.println(payload);             //レスポンスペイロードを表示

            // 特定のパートを解析する、例えば、ペイロードからタイトルを抽出
            int titleStart = payload.indexOf("<title>") + 7; // "<title>"タグを超えて進むために+7
            int titleEnd = payload.indexOf("</title>", titleStart);
            String pageTitle = payload.substring(titleStart, titleEnd);

            Serial.print("ページタイトル: ");
            Serial.println(pageTitle);
        }

        http.end();   //接続を閉じる
    }

    delay(10000); //10秒ごとにリクエストを行う
}
```

サンプル出力（http://example.com がシンプルなHTML構造を持っていると仮定）：
```
接続中...
...
ページタイトル: Example Domain
```

この例は、HTMLページを取得して`<title>`タグの内容を抽出する方法を示しています。より複雑なHTMLの解析には、メモリの制約に注意しながら正規表現を使うか、HTML構造を通過するための文字列操作機能を使うことを検討してください。高度な解析では、標準のArduino環境には組み込みのHTML解析ライブラリが含まれていないため、対処しているHTMLの特定の構造に合わせてカスタマイズされた解析アルゴリズムを含むより洗練されたアプローチが必要となる場合があります。
