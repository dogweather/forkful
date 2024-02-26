---
date: 2024-01-20 18:01:33.051710-07:00
description: "\u4F55\u3068\u306A\u305C\uFF1F HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\
  \u57FA\u672C\u8A8D\u8A3C\u4ED8\u304D\u3067\u9001\u308B\u3068\u306F\u3001\u30E6\u30FC\
  \u30B6\u540D\u3068\u30D1\u30B9\u30EF\u30FC\u30C9\u3067\u4FDD\u8B77\u3055\u308C\u305F\
  \u30EA\u30BD\u30FC\u30B9\u306B\u30A2\u30AF\u30BB\u30B9\u3059\u308B\u305F\u3081\u306E\
  \u624B\u6BB5\u3067\u3059\u3002\u3053\u308C\u306B\u3088\u308A\u3001\u5236\u9650\u3055\
  \u308C\u305F\u30C7\u30FC\u30BF\u306B\u78BA\u5B9F\u306B\u5B89\u5168\u306B\u30A2\u30AF\
  \u30BB\u30B9\u3059\u308B\u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-02-25T18:49:40.461099-07:00'
model: gpt-4-1106-preview
summary: "\u4F55\u3068\u306A\u305C\uFF1F HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u57FA\
  \u672C\u8A8D\u8A3C\u4ED8\u304D\u3067\u9001\u308B\u3068\u306F\u3001\u30E6\u30FC\u30B6\
  \u540D\u3068\u30D1\u30B9\u30EF\u30FC\u30C9\u3067\u4FDD\u8B77\u3055\u308C\u305F\u30EA\
  \u30BD\u30FC\u30B9\u306B\u30A2\u30AF\u30BB\u30B9\u3059\u308B\u305F\u3081\u306E\u624B\
  \u6BB5\u3067\u3059\u3002\u3053\u308C\u306B\u3088\u308A\u3001\u5236\u9650\u3055\u308C\
  \u305F\u30C7\u30FC\u30BF\u306B\u78BA\u5B9F\u306B\u5B89\u5168\u306B\u30A2\u30AF\u30BB\
  \u30B9\u3059\u308B\u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002"
title: "\u57FA\u672C\u8A8D\u8A3C\u3092\u4F7F\u7528\u3057\u305FHTTP\u30EA\u30AF\u30A8\
  \u30B9\u30C8\u306E\u9001\u4FE1"
---

{{< edit_this_page >}}

## What & Why?
何となぜ？

HTTPリクエストを基本認証付きで送るとは、ユーザ名とパスワードで保護されたリソースにアクセスするための手段です。これにより、制限されたデータに確実に安全にアクセスすることができます。

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
