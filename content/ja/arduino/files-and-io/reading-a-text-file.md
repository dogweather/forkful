---
title:                "テキストファイルの読み込み"
aliases:
- /ja/arduino/reading-a-text-file/
date:                  2024-01-20T17:53:51.952205-07:00
model:                 gpt-4-1106-preview
simple_title:         "テキストファイルの読み込み"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/reading-a-text-file.md"
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
