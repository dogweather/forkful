---
title:                "HTTPリクエストの送信"
html_title:           "Bash: HTTPリクエストの送信"
simple_title:         "HTTPリクエストの送信"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 何となぜ？

HTTPリクエストの送信とは、ウェブサーバーに情報をリクエストまたは送信するプロセスのことを指します。これにより、プログラマーはウェブ上の別のシステムと通信し、データを取得または更新することができます。

## 実行方法

Arduinoとウェブサーバとの間でHTTPリクエストを送信するための基本的なコードは以下の通りです。

```Arduino
#include <WiFi.h>

const char* ssid     = "your_SSID";
const char* password = "your_PASSWORD";

const char* host = "maker.ifttt.com";

void setup() {
  Serial.begin(115200);
  delay(10);

  // We start by connecting to a WiFi network

  Serial.println();
  Serial.print("Connecting to ");
  Serial.println(ssid);

  WiFi.begin(ssid, password);

  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.print(".");
  }

  Serial.println("");
  Serial.println("WiFi connected");  
  Serial.println("IP address: ");
  Serial.println(WiFi.localIP());
}

void loop() {
  WiFiClient client;
  const int httpPort = 80;
  if (!client.connect(host, httpPort)) {
    Serial.println("connection failed");
    return;
  }

  String url = "/trigger/event/with/key/your_key";

  Serial.print("Requesting URL: ");
  Serial.println(url);
  
  client.print(String("GET ") + url + " HTTP/1.1\r\n" +
               "Host: " + host + "\r\n" + 
               "Connection: close\r\n\r\n");
  delay(10);

  while(client.available()){
    String line = client.readStringUntil('\r');
    Serial.print(line);
  }
}
```

これでサーバーへのHTTPリクエストが可能になります。

## ディープダイブ

HTTPリクエストの送信は、初期のインターネットの時代、つまり90年代初頭から存在します。それはウェブページへのアクセスに使用され、ちょうど今日のArduinoで使用されているように、デバイス間で情報を交換する手段となりました。

このコードの代わりに使用可能な代替手段としては、HTTPSを使用した安全なリクエストの送信や、UDPを使用したデータの送信などがあります。

この実装に関しては、WiFi.hライブラリを使用して、Arduinoがインターネットに接続できるようにしました。そして、その接続を使ってHTTPリクエストを作成し、特定のURLに対してそれを送信しています。

## 参考資料

2. WiFi.hライブラリのドキュメンテーション：[https://www.arduino.cc/en/Reference/WiFi](https://www.arduino.cc/en/Reference/WiFi)