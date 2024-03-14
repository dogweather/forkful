---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:54.371913-07:00
description: "Arduino\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\u3067\u306EHTML\u306E\u89E3\
  \u6790\u3068\u306F\u3001\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u304B\u3089\u60C5\u5831\
  \u3092\u62BD\u51FA\u3059\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u30FC\u306F\u3001Arduino\u30C7\u30D0\u30A4\u30B9\u304C\u30A4\u30F3\u30BF\u30FC\
  \u30CD\u30C3\u30C8\u3068\u5BFE\u8A71\u3067\u304D\u308B\u3088\u3046\u306B\u3059\u308B\
  \u305F\u3081\u3001\u30DB\u30FC\u30E0\u30AA\u30FC\u30C8\u30E1\u30FC\u30B7\u30E7\u30F3\
  \u304B\u3089\u74B0\u5883\u30E2\u30CB\u30BF\u30EA\u30F3\u30B0\u307E\u3067\u306E\u76EE\
  \u7684\u3067\u30A6\u30A7\u30D6\u30B5\u30A4\u30C8\u304B\u3089\u30C7\u30FC\u30BF\u3092\
  \u53CE\u96C6\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\
  \u3002"
lastmod: '2024-03-13T22:44:42.494666-06:00'
model: gpt-4-0125-preview
summary: "Arduino\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\u3067\u306EHTML\u306E\u89E3\u6790\
  \u3068\u306F\u3001\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u304B\u3089\u60C5\u5831\u3092\
  \u62BD\u51FA\u3059\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u306F\u3001Arduino\u30C7\u30D0\u30A4\u30B9\u304C\u30A4\u30F3\u30BF\u30FC\u30CD\
  \u30C3\u30C8\u3068\u5BFE\u8A71\u3067\u304D\u308B\u3088\u3046\u306B\u3059\u308B\u305F\
  \u3081\u3001\u30DB\u30FC\u30E0\u30AA\u30FC\u30C8\u30E1\u30FC\u30B7\u30E7\u30F3\u304B\
  \u3089\u74B0\u5883\u30E2\u30CB\u30BF\u30EA\u30F3\u30B0\u307E\u3067\u306E\u76EE\u7684\
  \u3067\u30A6\u30A7\u30D6\u30B5\u30A4\u30C8\u304B\u3089\u30C7\u30FC\u30BF\u3092\u53CE\
  \u96C6\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
title: "HTML\u306E\u89E3\u6790"
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
