---
title:                "テキストファイルの書き込み"
date:                  2024-01-19
simple_title:         "テキストファイルの書き込み"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
テキストファイルを書くって何？とにかく重要な設定やデータを保存することだよ。プログラマはなぜこれをやるの？再起動後も情報を取り戻せるからだね。

## How to: (やり方)
Arduinoでテキストファイルを書こう。ここでSDカードモジュールを使う例を見てみよう。忘れずに、`SD.h`ライブラリを読み込もうね。

```Arduino
#include <SD.h>

File myFile;

void setup() {
  Serial.begin(9600);

  if (!SD.begin(4)) {
    Serial.println("SDカードの初期化失敗");
    return;
  }
  
  myFile = SD.open("test.txt", FILE_WRITE);
  
  if (myFile) {
    myFile.println("こんにちは, Arduino!");
    myFile.close(); 
    Serial.println("ファイルに書き込みました。");
  } else {
    Serial.println("ファイルを開けませんでした。");
  }
}

void loop() {
  // 空
}
```
このコードは`test.txt`に"こんにちは, Arduino!"と書き込むよ。

## Deep Dive (深堀り)
テキストファイルの書き込みはSDカードやEEPROMに使われる歴史がある。選択肢としては、Flashメモリやクラウドサービスに送信する方法もあるぞ。ファイルシステムの制限やSDカードの速度がパフォーマンスに影響を与えることも頭に入れておこう。

## See Also (関連項目)
- Arduinoの`SD`ライブラリドキュメント: https://www.arduino.cc/en/reference/SD
- より詳しいEEPROMの使用方法: https://www.arduino.cc/en/Tutorial/LibraryExamples/EEPROMReadWrite
- ArduinoでのファイルI/Oチュートリアル: https://www.arduino.cc/en/Tutorial/LibraryExamples/ReadWrite
- SPIに関する情報: https://www.arduino.cc/en/reference/SPI
