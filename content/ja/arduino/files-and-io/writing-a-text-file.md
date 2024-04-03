---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:21.526566-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:42.519548-06:00'
model: gpt-4-0125-preview
summary: "Arduino\u3067\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u3092\u66F8\
  \u304D\u8FBC\u3080\u3053\u3068\u306F\u3001SD\u30AB\u30FC\u30C9\u3084\u985E\u4F3C\
  \u306E\u30B9\u30C8\u30EC\u30FC\u30B8\u30E2\u30B8\u30E5\u30FC\u30EB\u4E0A\u306E\u30D5\
  \u30A1\u30A4\u30EB\u306B\u30C7\u30FC\u30BF\u3092\u4FDD\u5B58\u3059\u308B\u3053\u3068\
  \u3092\u6307\u3057\u307E\u3059\u3002\u3053\u308C\u306F\u3001\u3057\u3070\u3057\u3070\
  \u30C7\u30FC\u30BF\u30ED\u30B0\u306E\u76EE\u7684\u3067\u884C\u308F\u308C\u307E\u3059\
  \u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30BB\u30F3\u30B5\u30FC\u306E\
  \u8AAD\u307F\u53D6\u308A\u5024\u3092\u8A18\u9332\u3057\u305F\u308A\u3001\u8A2D\u5B9A\
  \u3092\u4FDD\u5B58\u3057\u305F\u308A\u3001\u6642\u9593\u3092\u304B\u3051\u3066\u30A2\
  \u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u30A4\u30D9\u30F3\u30C8\u3092\u30ED\u30B0\
  \u306B\u8A18\u9332\u3057\u305F\u308A\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\
  \u884C\u3044\u307E\u3059\u3002\u3053\u308C\u306F\u3001\u30C7\u30FC\u30BF\u5206\u6790\
  \u3084\u8FFD\u8DE1\u3092\u5FC5\u8981\u3068\u3059\u308B\u30D7\u30ED\u30B8\u30A7\u30AF\
  \u30C8\u306B\u3068\u3063\u3066\u91CD\u8981\u3067\u3059\u3002."
title: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u4F5C\u6210"
weight: 24
---

## 何を、なぜ？
Arduinoでテキストファイルを書き込むことは、SDカードや類似のストレージモジュール上のファイルにデータを保存することを指します。これは、しばしばデータログの目的で行われます。プログラマーは、センサーの読み取り値を記録したり、設定を保存したり、時間をかけてアプリケーションイベントをログに記録したりするためにこれを行います。これは、データ分析や追跡を必要とするプロジェクトにとって重要です。

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
