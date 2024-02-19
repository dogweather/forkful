---
aliases:
- /ja/arduino/reading-a-text-file/
date: 2024-01-20 17:53:51.952205-07:00
description: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u3092\u8AAD\u3080\u3063\
  \u3066\u3044\u3046\u306E\u306F\u3001\u5358\u306B\u30D5\u30A1\u30A4\u30EB\u304B\u3089\
  \u6587\u5B57\u30C7\u30FC\u30BF\u3092\u53D6\u308A\u51FA\u3059\u3053\u3068\u3060\u3002\
  \u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u8A2D\u5B9A\u3001\u30C7\u30FC\u30BF\u306E\
  \u53CE\u96C6\u3001\u3042\u308B\u3044\u306F\u7C21\u5358\u306A\u901A\u4FE1\u306E\u305F\
  \u3081\u306B\u3053\u308C\u3092\u3088\u304F\u884C\u3046\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:55.168383
model: gpt-4-1106-preview
summary: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u3092\u8AAD\u3080\u3063\
  \u3066\u3044\u3046\u306E\u306F\u3001\u5358\u306B\u30D5\u30A1\u30A4\u30EB\u304B\u3089\
  \u6587\u5B57\u30C7\u30FC\u30BF\u3092\u53D6\u308A\u51FA\u3059\u3053\u3068\u3060\u3002\
  \u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u8A2D\u5B9A\u3001\u30C7\u30FC\u30BF\u306E\
  \u53CE\u96C6\u3001\u3042\u308B\u3044\u306F\u7C21\u5358\u306A\u901A\u4FE1\u306E\u305F\
  \u3081\u306B\u3053\u308C\u3092\u3088\u304F\u884C\u3046\u3002"
title: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u8AAD\u307F\u8FBC\u307F"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)

テキストファイルを読むっていうのは、単にファイルから文字データを取り出すことだ。プログラマーは設定、データの収集、あるいは簡単な通信のためにこれをよく行う。

## How to: (方法)

以下に、Arduinoでテキストファイルを読み込む簡単なコードを示します。先にmicroSDカードに接続されたSDカードモジュールが必要です。

```Arduino
#include <SD.h>
#include <SPI.h>

File myFile;

void setup() {
  Serial.begin(9600);
  while (!Serial) {
    ; // 待機してシリアルポートが開くのを待つ
  }

  if (!SD.begin(4)) {
    Serial.println("SDカードの初期化に失敗しました。");
    return;
  }
  
  myFile = SD.open("example.txt");
  if (myFile) {
    while (myFile.available()) {
      Serial.write(myFile.read());
    }
    myFile.close();
  } else {
    Serial.println("ファイルを開けませんでした。");
  }
}

void loop() {
  // ここには何も書かない
}
```
このコードは"example.txt"の内容をシリアルモニタに出力します。

## Deep Dive (深堀り)

テキストファイルを読む機能は、最初のコンピューターが登場して以来のもの。ファイルシステムはOSの大事な部分。Arduinoにおいては、通常SDカードモジュールを使用して実現する。SPI通信を経由し、`SD`ライブラリが利用される。他の方法としては、EEPROMやフラッシュメモリの使用もあるが、容量が大きなファイルではSDカードが適している。実装上の詳細では、`File`オブジェクト、`open`、`available`、`read`、`close`メソッドの使い方が重要。

## See Also (参照)

- [Arduinoの公式SDライブラリドキュメント](https://www.arduino.cc/en/Reference/SD)
- [SPI通信についての解説](https://www.arduino.cc/en/reference/SPI)
- [フラッシュメモリとEEPROMの使用](https://www.arduino.cc/en/Reference/EEPROM)

読み込むファイルの種類やSDカードのセットアップ手順についてもっと学びたければ、これらのリンクをチェックしてみてください。
