---
date: 2024-01-20 18:01:33.051710-07:00
description: "How to: \u6DF1\u6398\u308A\uFF1A HTTP\u57FA\u672C\u8A8D\u8A3C\u306F\u3001\
  1989\u5E74\u306B\u767A\u660E\u3055\u308C\u305FWeb\u306E\u521D\u671F\u304B\u3089\u5B58\
  \u5728\u3057\u307E\u3059\u3002\u3053\u308C\u306F\u3001HTTP\u30D7\u30ED\u30C8\u30B3\
  \u30EB\u306E\u8A8D\u8A3C\u30E1\u30AB\u30CB\u30BA\u30E0\u306E\u4E00\u3064\u3067\u3001\
  \u30E6\u30FC\u30B6\u540D\u3068\u30D1\u30B9\u30EF\u30FC\u30C9\u3092Base64\u3067\u30A8\
  \u30F3\u30B3\u30FC\u30C9\u3057\u3066\u9001\u4FE1\u3057\u307E\u3059\u3002HTTPS\u3068\
  \u4F75\u7528\u3059\u308B\u3053\u3068\u3067\u3001\u60C5\u5831\u304C\u6697\u53F7\u5316\
  \u3055\u308C\u308B\u305F\u3081\u3001\u4FE1\u983C\u6027\u304C\u9AD8\u307E\u308A\u307E\
  \u3059\u3002\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:38:42.004669-06:00'
model: gpt-4-1106-preview
summary: "\u6DF1\u6398\u308A\uFF1A HTTP\u57FA\u672C\u8A8D\u8A3C\u306F\u30011989\u5E74\
  \u306B\u767A\u660E\u3055\u308C\u305FWeb\u306E\u521D\u671F\u304B\u3089\u5B58\u5728\
  \u3057\u307E\u3059\u3002\u3053\u308C\u306F\u3001HTTP\u30D7\u30ED\u30C8\u30B3\u30EB\
  \u306E\u8A8D\u8A3C\u30E1\u30AB\u30CB\u30BA\u30E0\u306E\u4E00\u3064\u3067\u3001\u30E6\
  \u30FC\u30B6\u540D\u3068\u30D1\u30B9\u30EF\u30FC\u30C9\u3092Base64\u3067\u30A8\u30F3\
  \u30B3\u30FC\u30C9\u3057\u3066\u9001\u4FE1\u3057\u307E\u3059\u3002HTTPS\u3068\u4F75\
  \u7528\u3059\u308B\u3053\u3068\u3067\u3001\u60C5\u5831\u304C\u6697\u53F7\u5316\u3055\
  \u308C\u308B\u305F\u3081\u3001\u4FE1\u983C\u6027\u304C\u9AD8\u307E\u308A\u307E\u3059\
  \u3002"
title: "\u57FA\u672C\u8A8D\u8A3C\u3092\u4F7F\u7528\u3057\u305FHTTP\u30EA\u30AF\u30A8\
  \u30B9\u30C8\u306E\u9001\u4FE1"
weight: 45
---

## How to:
やり方：

```Arduino
#include <ESP8266WiFi.h>
#include <Base64.h>

const char* ssid     = "yourSSID";     // WiFiのSSID
const char* password = "yourPassword"; // WiFiのパスワード
const char* server   = "server.com";   // サーバのドメインまたはIPアドレス

const char* httpUser = "yourUsername"; // HTTPのユーザ名
const char* httpPass = "yourHTTPPass"; // HTTPのパスワード

WiFiClient client;

void setup() {
  Serial.begin(115200);
  
  // WiFi接続
  WiFi.begin(ssid, password);
  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.print(".");
  }
  Serial.println("Connected to WiFi");
  
  // HTTPリクエストの送信
  if (client.connect(server, 80)) {
    String authValue = "Basic " + base64::encode(String(httpUser) + ":" + String(httpPass));
    
    client.println("GET /restricted/data HTTP/1.1");
    client.println("Host: " + String(server));
    client.println("Authorization: " + authValue);
    client.println("Connection: close");
    client.println();
  }
}

void loop() {
  // サーバからの応答があるまで待機
  while(client.available()) {
    String line = client.readStringUntil('\r');
    Serial.print(line);
  }
  
  // 応答の読み込みが完了したら切断
  if (!client.connected()) {
    Serial.println();
    Serial.println("Disconnecting from server...");
    client.stop();
    
    // 再度接続しなくてもよいので、ループから抜け出す
    while(true) {
      delay(1000);
    }
  }
}
```

## Deep Dive:
深掘り：

HTTP基本認証は、1989年に発明されたWebの初期から存在します。これは、HTTPプロトコルの認証メカニズムの一つで、ユーザ名とパスワードをBase64でエンコードして送信します。HTTPSと併用することで、情報が暗号化されるため、信頼性が高まります。

代替手段としてOAuthやAPIキーなど他の認証方式がありますが、シンプルさと簡易な実装のために基本認証はまだ利用されています。

ESP8266などのマイクロコントローラを使用する場合、リクエストのヘッダに認証情報を追加する必要があります。Arduino環境でのHTTP通信には、`ESP8266HTTPClient`ライブラリもありますが、今回はそのライブラリを使わずに直接TCP接続を行っています。

## See Also:
関連する情報：

- Arduino公式サイトのHTTPClientライブラリ: https://www.arduino.cc/en/Reference/HTTPClient
- Base64エンコーディングについて: https://www.base64encode.org/
- よりセキュアな認証方法としてのOAuth: https://oauth.net/
