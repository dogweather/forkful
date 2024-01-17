---
title:                "ウェブページのダウンロード"
html_title:           "Arduino: ウェブページのダウンロード"
simple_title:         "ウェブページのダウンロード"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 何と、なぜするの？

ウェブページをダウンロードするとは、インターネットから特定のウェブページを取得することです。プログラマーは主に、ウェブサイトから必要なデータを取得するためにこれを行います。

##  方法：

```
Arduinoを使用し、ウェブページをダウンロードする方法はいくつかあります。まず、ウェブサーバーへの接続を確立する必要があります。次に、HTTPリクエストを使用して特定のページを指定し、サーバーからデータを取得します。最後に、必要なデータを処理し、使用する形式に変換します。以下は、ダウンロードしたウェブページをシリアルモニターに表示する例です。

```Arduino
// ウェブサーバーへの接続を確立する
WiFiClient client;
if (!client.connect(server, 80)) {
  Serial.println("接続に失敗しました");
  return;
}

// ページを要求する
client.println("GET /index.html HTTP/1.1");
client.println("Host: www.example.com");
client.println("Connection: close");
client.println();

// ページのデータを取得する
while(client.available()){
  char c = client.read();
  Serial.print(c);
}

// 接続を閉じる
client.stop();
```

## ディープダイブ：

ウェブページをダウンロードする方法には、さまざまなアプローチがあります。Arduino以外にも、例えばESP32やESP8266を使用することもできます。また、HTTP以外のプロトコルを使用することも可能です。さらに、ウェブスクレイピングと呼ばれる、ウェブサイトからデータを収集する方法もあります。

## 関連リンク：

- [ウェブスクレイピングについての記事](https://qiita.com/manabuyasuda/items/edb7e6f69622c9ccf28d)
- [ESP32を使用したウェブページのダウンロードの方法についてのチュートリアル](https://randomnerdtutorials.com/esp32-web-server-getting-query-parameters/)
- [ESP8266を使用したウェブページのダウンロードの方法についてのチュートリアル](https://circuitdigest.com/microcontroller-projects/esp8266-based-web-page-download-to-sd-card)