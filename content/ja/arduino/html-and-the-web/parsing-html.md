---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:54.371913-07:00
description: "\u65B9\u6CD5: Arduino\u3067\u306EHTML\u306E\u89E3\u6790\u306B\u306F\u3001\
  \u901A\u5E38\u3001\u9650\u3089\u308C\u305F\u30C7\u30D0\u30A4\u30B9\u30EA\u30BD\u30FC\
  \u30B9\u306E\u305F\u3081\u306B\u6700\u5C0F\u9650\u306E\u30D5\u30C3\u30C8\u30D7\u30EA\
  \u30F3\u30C8\u30E9\u30A4\u30D6\u30E9\u30EA\u304C\u5FC5\u8981\u3067\u3059\u3002\u30A6\
  \u30A7\u30D6\u30B9\u30AF\u30EC\u30A4\u30D4\u30F3\u30B0\u3068\u89E3\u6790\u306B\u4EBA\
  \u6C17\u306A\u9078\u629E\u80A2\u306F\u3001Wi-\u2026"
lastmod: '2024-04-05T21:53:43.309198-06:00'
model: gpt-4-0125-preview
summary: "Arduino\u3067\u306EHTML\u306E\u89E3\u6790\u306B\u306F\u3001\u901A\u5E38\u3001\
  \u9650\u3089\u308C\u305F\u30C7\u30D0\u30A4\u30B9\u30EA\u30BD\u30FC\u30B9\u306E\u305F\
  \u3081\u306B\u6700\u5C0F\u9650\u306E\u30D5\u30C3\u30C8\u30D7\u30EA\u30F3\u30C8\u30E9\
  \u30A4\u30D6\u30E9\u30EA\u304C\u5FC5\u8981\u3067\u3059\u3002\u30A6\u30A7\u30D6\u30B9\
  \u30AF\u30EC\u30A4\u30D4\u30F3\u30B0\u3068\u89E3\u6790\u306B\u4EBA\u6C17\u306A\u9078\
  \u629E\u80A2\u306F\u3001Wi-Fi\u6A5F\u80FD\u3068HTTP\u30D7\u30ED\u30C8\u30B3\u30EB\
  \u306E\u30CD\u30A4\u30C6\u30A3\u30D6\u30B5\u30DD\u30FC\u30C8\u3092\u8003\u616E\u3057\
  \u3066\u3001ESP8266\u7528\u306E`ESP8266HTTPClient`\u3068`ESP8266WiFi`\u30E9\u30A4\
  \u30D6\u30E9\u30EA\u3001\u3042\u308B\u3044\u306F\u305D\u308C\u3089\u306EESP32\u30D0\
  \u30FC\u30B8\u30E7\u30F3\u3092\u4F7F\u7528\u3059\u308B\u3053\u3068\u3067\u3059\u3002\
  \u3053\u3053\u3067\u306F\u3001ESP8266\u3084ESP32\u3092\u4F7F\u7528\u3057\u3066\u3044\
  \u308B\u3068\u4EEE\u5B9A\u3057\u3066\u3001HTML\u3092\u53D6\u5F97\u3057\u89E3\u6790\
  \u3059\u308B\u57FA\u672C\u4F8B\u3092\u793A\u3057\u307E\u3059\uFF1A \u307E\u305A\u3001\
  \u5FC5\u8981\u306A\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u542B\u3080\uFF1A."
title: "HTML\u306E\u89E3\u6790"
weight: 43
---

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
