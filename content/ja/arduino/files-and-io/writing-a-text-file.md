---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:21.526566-07:00
description: "\u65B9\u6CD5\uFF1A Arduino\u3092\u4F7F\u7528\u3057\u3066SD\u30AB\u30FC\
  \u30C9\u4E0A\u306E\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306B\u66F8\u304D\
  \u8FBC\u3080\u306B\u306F\u3001\u307E\u305A`SD.h`\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\
  \u542B\u3081\u308B\u5FC5\u8981\u304C\u3042\u308A\u307E\u3059\u3002\u3053\u308C\u306F\
  \u3001SD\u30AB\u30FC\u30C9\u3068\u3084\u308A\u53D6\u308A\u3059\u308B\u305F\u3081\
  \u306E\u5FC5\u8981\u306A\u95A2\u6570\u3092\u63D0\u4F9B\u3057\u307E\u3059\u3002Arduino\u30DC\
  \u30FC\u30C9\u304CSD\u30AB\u30FC\u30C9\u30E2\u30B8\u30E5\u30FC\u30EB\u306B\u63A5\
  \u7D9A\u3055\u308C\u3066\u3044\u308B\u3053\u3068\u3092\u78BA\u8A8D\u3057\u3066\u304F\
  \u3060\u3055\u3044\u3002"
lastmod: '2024-03-13T22:44:42.519548-06:00'
model: gpt-4-0125-preview
summary: "Arduino\u3092\u4F7F\u7528\u3057\u3066SD\u30AB\u30FC\u30C9\u4E0A\u306E\u30C6\
  \u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306B\u66F8\u304D\u8FBC\u3080\u306B\u306F\
  \u3001\u307E\u305A`SD.h`\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u542B\u3081\u308B\u5FC5\
  \u8981\u304C\u3042\u308A\u307E\u3059\u3002\u3053\u308C\u306F\u3001SD\u30AB\u30FC\
  \u30C9\u3068\u3084\u308A\u53D6\u308A\u3059\u308B\u305F\u3081\u306E\u5FC5\u8981\u306A\
  \u95A2\u6570\u3092\u63D0\u4F9B\u3057\u307E\u3059\u3002Arduino\u30DC\u30FC\u30C9\u304C\
  SD\u30AB\u30FC\u30C9\u30E2\u30B8\u30E5\u30FC\u30EB\u306B\u63A5\u7D9A\u3055\u308C\
  \u3066\u3044\u308B\u3053\u3068\u3092\u78BA\u8A8D\u3057\u3066\u304F\u3060\u3055\u3044\
  ."
title: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u4F5C\u6210"
weight: 24
---

## 方法：
Arduinoを使用してSDカード上のテキストファイルに書き込むには、まず`SD.h`ライブラリを含める必要があります。これは、SDカードとやり取りするための必要な関数を提供します。ArduinoボードがSDカードモジュールに接続されていることを確認してください。

```cpp
#include <SPI.h>
#include <SD.h>

File myFile;

void setup() {
  // 9600ビット/秒でシリアル通信を初期化する：
  Serial.begin(9600);
  
  // SDカードの初期化をチェック
  if (!SD.begin(4)) {
    Serial.println("Initialization failed!");
    return;
  }
  Serial.println("Initialization done.");
  
  // ファイルを開く。一度に開けるファイルは1つだけであることに注意し、
  // 別のものを開く前にこのファイルを閉じる必要がある。
  myFile = SD.open("test.txt", FILE_WRITE);
  
  // ファイルがうまく開いたら、書き込む：
  if (myFile) {
    Serial.print("Writing to test.txt...");
    myFile.println("Testing text file write.");
    // ファイルを閉じる：
    myFile.close();
    Serial.println("done.");
  } else {
    // ファイルが開かなかった場合、エラーを出力する：
    Serial.println("Error opening test.txt");
  }
}

void loop() {
  // setup後には何も起こらない
}
```

### サンプル出力：
このコードを実行すると、Arduino IDEのシリアルモニターに以下が表示されます：
```
Initialization done.
Writing to test.txt...done.
```
データが正しく書き込まれたかを確認するには、ArduinoからSDカードを取り外し、コンピュータに挿入して`test.txt`ファイルを開き、"Testing text file write."というメッセージを確認します。

より高度なファイル操作や処理を必要とするプロジェクトには、追加のライブラリを探索するか、特定のニーズに合わせたカスタム関数を書くことを検討してください。
