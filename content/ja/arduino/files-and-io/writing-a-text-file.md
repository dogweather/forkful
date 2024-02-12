---
title:                "テキストファイルの作成"
aliases:
- /ja/arduino/writing-a-text-file.md
date:                  2024-02-03T19:27:21.526566-07:00
model:                 gpt-4-0125-preview
simple_title:         "テキストファイルの作成"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/writing-a-text-file.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
