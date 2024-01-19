---
title:                "ウェブページのダウンロード"
html_title:           "Bash: ウェブページのダウンロード"
simple_title:         "ウェブページのダウンロード"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# 何となぜ？ (What & Why?)
ウェブページをダウンロードするとは、ウェブサーバーから情報を取得し、自分のデバイスに保存することです。プログラマーはこれを行うことで、オフラインでアクセス可能なデータを提供したり、特定の情報を取得したりします。

# どうやって：(How To:)
以下は、Arduinoでウェブページをダウンロードするための基本的なコードです。

```Arduino
#include <Ethernet.h>
#include <SPI.h>

byte mac[] = { 0xDE, 0xAD, 0xBE, 0xEF, 0xFE, 0xED };
char server[] = "www.example.com";

EthernetClient client;

void setup()
{
  Ethernet.begin(mac);
  Serial.begin(9600);

  if (client.connect(server, 80)) {
    Serial.println("connected");
    client.println("GET / HTTP/1.1");
    client.println("Host: www.example.com");
    client.println("Connection: close");
    client.println();
  }
  else {
    Serial.println("connection failed");
  }
}

void loop()
{
  if (client.available()) {
    char c = client.read();
    Serial.print(c);
  }
  
  if (!client.connected()) {
    client.stop();
    for(;;)
      ;
  }
}
```
このコードはwww.example.comのホームページをHTTP GETリクエストを使ってダウンロードします。

# ディープダイブ (Deep Dive)
インターネットの初期段階では、ウェブページのダウンロードはFTPを使用して行われていた代わりに、今日ではHTTPが主流となっています。Arduinoでは、今上記で説明したEthernetライブラリのほか、WiFiライブラリも利用可能で、具体的なライブラリの選択はプロジェクトのニーズや使用しているハードウェアによります。

データの読み取り方も重要です。上記の方法は大量のデータに対しては非効率的で、実際にはデータの流れを管理しながらページをダウンロードするための追加コードが必要です。

# さらに見るべきもの (See Also)
1. Arduino Ethernet ライブラリ: https://www.arduino.cc/en/Tutorial/LibraryExamples/Ethernet 
2. Arduino WiFi ライブラリ: https://www.arduino.cc/en/Reference/WiFi 
3. HTTPの概要: https://www.w3.org/Protocols/