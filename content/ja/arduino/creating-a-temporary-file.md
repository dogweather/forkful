---
title:    "Arduino: 一時ファイルの作成"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## なぜ

一時的なファイルを作成することの利点は、非常に便利であることです。プログラマーは、データを一時的に保存することができます。また、プログラムを実行する際に、一時的なファイルを使用することにより、メモリの使用量を減らすことができます。

## 作り方

一時的なファイルを作成するには、ArduinoのFileクラスを使用することができます。これにより、ファイルオブジェクトを作成し、その中で一時的なファイルを作成することができます。以下の例を参考にしてください。

```Arduino
#include <SD.h>

File myFile;

// 一時的なファイルを作成して、"temp.txt"に書き込む
void createTempFile(){
  myFile = SD.open("temp.txt", FILE_WRITE);
  if (myFile) {
    myFile.println("This is a temporary file.");
    myFile.close();
  } else {
    // エラー処理
  }
}

// 一時的なファイルを読み込んでシリアルモニターに出力する
void readTempFile(){
  myFile = SD.open("temp.txt");
  if (myFile) {
    while (myFile.available()) {
      Serial.println(myFile.read());
    }
    myFile.close();
  } else {
    // エラー処理
  }
}

void setup() {
  Serial.begin(9600);
  // SDカードの初期化
  if (!SD.begin()) {
    // エラー処理
  }
  createTempFile();
  readTempFile();
}

void loop() {
  //何もしない
}
```

実行すると、シリアルモニターに「This is a temporary file.」というテキストが表示されます。これが一時的なファイルから読み込まれたものです。

## 深い掘り下げ

一時的なファイルには、ファイル名を付けなくても作成することができます。その場合、SDライブラリは自動的に一意のファイル名を付けます。また、一時的なファイルはプログラムの終了時に自動的に削除されます。これにより、メモリの使用量を管理することができます。

## また見る

- [Arduino SDライブラリのドキュメント](https://www.arduino.cc/en/Reference/SD)
- [Fileクラスのドキュメント](https://www.arduino.cc/en/Reference/SDFile)
- [一時的なファイルの作成方法（英語）](https://stackoverflow.com/questions/423802/creating-a-temporary-file-in-arduino)