---
title:                "基本認証を使用してhttpリクエストを送信する"
html_title:           "C#: 基本認証を使用してhttpリクエストを送信する"
simple_title:         "基本認証を使用してhttpリクエストを送信する"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 何となぜ？

HTTPリクエストに基本認証を使用するとは、ウェブサーバーからデータを取得したり、送信する際の安全な手段です。これにより、プログラマーは機密データを安全に転送できます。

## どうやるか：

以下はArduinoによる基本認証を使用したHTTPリクエストのサンプルコードです：

```Arduino
#include <ESP8266WiFi.h>
#include <ESP8266HTTPClient.h>

const char* ssid = "your_SSID";
const char* password = "your_PASSWORD";

void setup () {

  Serial.begin(115200);
  WiFi.begin(ssid, password);

  while (WiFi.status() != WL_CONNECTED) {

    delay(1000);
    Serial.print("Connecting..");

  }
}

void loop() {
  
  if (WiFi.status() == WL_CONNECTED) { 
    HTTPClient http;  
      
    http.begin("http://yourserver.com");  
    http.addHeader("Content-Type", "text/plain");  //Specify content-type header
    http.setAuthorization("username","password");  //Specify basic authentication
    
    int httpCode = http.GET();   
                                               
   
    if (httpCode > 0) { 

        String payload = http.getString();
        Serial.println(payload);                     
      
      }
    
    http.end();   
  }

  delay(30000);

}
```
このサンプルでは、Arduinoが30秒おきにHTTPリクエストを送信し、レスポンスをシリアルコンソールに出力します。

## ディープダイブ：

HTTPリクエストの基本認証は、ユーザー名とパスワードを一緒にBase64エンコードした「Authorization」ヘッダーとして送信します。このテクニックは、HTTP/1.0が初めて実装された1990年代から存在しています。

代替手段として、OAuthやJWTなどのより安全な認証方法がありますが、それらはセットアップがより複雑です。基本認証はシンプルで設定が容易なため、セキュリティが必要ない内部ネットワークでよく利用されます。

実装詳細については、ユーザー名とパスワードをBase64でエンコードする点が重要です。Arduinoでは、WiFiClientおよびHTTPClientライブラリを使用することで、HTTPリクエストと基本認証を容易に実装できます。

## 参考情報：

1. [Arduino公式](https://www.arduino.cc/en/Main/Software): Arduinoのダウンロードとドキュメンテーション。
2. [HTTPClientライブラリ](https://github.com/esp8266/Arduino/tree/master/libraries/ESP8266HTTPClient): HTTPリクエストを簡単に行うためのライブラリ。
3. [Basic Authentication on Wikipedia](https://en.wikipedia.org/wiki/Basic_access_authentication): 基本認証の詳細とその歴史。