---
title:                "基本認証を使用したHTTPリクエストの送信"
aliases:
- ja/arduino/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-20T18:01:33.051710-07:00
model:                 gpt-4-1106-preview
simple_title:         "基本認証を使用したHTTPリクエストの送信"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/sending-an-http-request-with-basic-authentication.md"
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
