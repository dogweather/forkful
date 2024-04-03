---
date: 2024-01-20 18:01:33.051710-07:00
description: "How to: \u3084\u308A\u65B9\uFF1A."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.497434-06:00'
model: gpt-4-1106-preview
summary: "\u3084\u308A\u65B9\uFF1A."
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
