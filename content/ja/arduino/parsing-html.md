---
title:                "HTMLの解析"
date:                  2024-01-20T15:29:55.672190-07:00
html_title:           "Arduino: HTMLの解析"
simple_title:         "HTMLの解析"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)

HTMLパース（解析）とはHTML文書からデータを抽出することです。ウェブページから特定の情報を自動的に取得したいときに行います。

## How to: (やり方：)

Arduinoでは直接HTMLをパースするライブラリは少ないですが、簡単なHTMLデータをパースするサンプルコードは以下の通りです。アウトプット例も見てみましょう。

```arduino
#include <Ethernet.h>
#include <EthernetClient.h>

EthernetClient client;

void setup() {
  Ethernet.begin(/* your MAC address */, /* your IP address */);
  Serial.begin(9600);
  if (client.connect(/* server's IP address */, 80)) {
    client.println("GET /path/to/page.html HTTP/1.1");
    client.println("Host: www.example.com");
    client.println("Connection: close");
    client.println();
  }
}

void loop() {
  while (client.available()) {
    String line = client.readStringUntil('\n');
    if (line.indexOf("<title>") > 0) {
      int start = line.indexOf("<title>") + 7;
      int end = line.indexOf("</title>");
      String title = line.substring(start, end);
      Serial.println(title);
    }
  }
}
```

サンプルのアウトプット:
```
Your Page Title
```

## Deep Dive (詳細解説）

ArduinoにおけるHTMLパースは通常のウェブ開発とは違います。メモリが限られているため、大きなHTMLドキュメントのパースは実用的ではありません。Arduinoの初期バージョンから、インターネット接続はEthernetやWiFiシールドを使ったり、ESP8266のような派生ボードの利用が一般的ですが、パーサはシンプルな文字列の検索に限られています。htmlparser.hなどのライブラリを探すときに利用可能性がありますが、これはArduinoの公式サポート外です。できるだけシンプルなタグを照会し、リソース消費を控えることが重要です。

## See Also (関連情報）

- Arduino Ethernet Library: https://www.arduino.cc/en/Reference/Ethernet
- Arduino String functions (for parsing strings): https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/
- Ethernet Shield: https://store.arduino.cc/usa/arduino-ethernet-shield-2
- ESP8266 Introduction: https://www.espressif.com/en/products/socs/esp8266
