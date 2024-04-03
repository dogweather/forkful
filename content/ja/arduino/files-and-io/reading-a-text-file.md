---
date: 2024-01-20 17:53:51.952205-07:00
description: "How to: (\u65B9\u6CD5) \u4EE5\u4E0B\u306B\u3001Arduino\u3067\u30C6\u30AD\
  \u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u3092\u8AAD\u307F\u8FBC\u3080\u7C21\u5358\u306A\
  \u30B3\u30FC\u30C9\u3092\u793A\u3057\u307E\u3059\u3002\u5148\u306BmicroSD\u30AB\u30FC\
  \u30C9\u306B\u63A5\u7D9A\u3055\u308C\u305FSD\u30AB\u30FC\u30C9\u30E2\u30B8\u30E5\
  \u30FC\u30EB\u304C\u5FC5\u8981\u3067\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.518696-06:00'
model: gpt-4-1106-preview
summary: "\u4EE5\u4E0B\u306B\u3001Arduino\u3067\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\
  \u30A4\u30EB\u3092\u8AAD\u307F\u8FBC\u3080\u7C21\u5358\u306A\u30B3\u30FC\u30C9\u3092\
  \u793A\u3057\u307E\u3059\u3002\u5148\u306BmicroSD\u30AB\u30FC\u30C9\u306B\u63A5\u7D9A\
  \u3055\u308C\u305FSD\u30AB\u30FC\u30C9\u30E2\u30B8\u30E5\u30FC\u30EB\u304C\u5FC5\
  \u8981\u3067\u3059."
title: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u8AAD\u307F\u8FBC\u307F"
weight: 22
---

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
