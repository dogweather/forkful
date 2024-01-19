---
title:                "テキストファイルの読み込み"
html_title:           "Bash: テキストファイルの読み込み"
simple_title:         "テキストファイルの読み込み"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 何となぜ? (What & Why?)
テキストファイルを読むとは、プログラムがテキストファイルの内容を収集することを指します。これは、データの保存や取得、単純なデーキュメント化のためにプログラマーによって行われる作業です。

## 方法: (How to:)
Arduinoを使用してテキストファイルからデータを読み込む方法を示します:

```Arduino
#include <SD.h>

File myFile;

void setup() {
  Serial.begin(9600);
  SD.begin(4);
  
  myFile = SD.open("test.txt");
  if (myFile) {
    while (myFile.available()) {
      Serial.write(myFile.read());
    }
    myFile.close();
  } else {
    Serial.println("File not found.");
  }
}

void loop() {

}
```

このコードは、ArduinoでSDカード上の "test.txt" ファイルを開き端末に内容を表示します。もしファイルが存在しない場合、「File not found.」と表示します。

## ディープダイブ (Deep Dive)
テキストファイルの読み込みは、プログラミングの初期から存在し、それを様々な方法で実装してきました。Arduinoでは上記のようにSDライブラリを使うことで実装が可能です。この方法は手軽でシンプルなため、多くのArduinoユーザーにとって第一の選択肢です。

代替手段として、ArduinoでFTPサーバーにアクセスしファイルを読み込むことも可能です。これはEthernetシールドを使用して実装されますが、より高度な知識を必要とします。

## 参考に (See Also):
1. Arduinoによるテキストファイルの読み書きの詳細: https://www.arduino.cc/en/Tutorial/LibraryExamples/ReadWrite 
2. Ethernetシールドを使用したFTPサーバーへのアクセス方法: https://www.arduino.cc/en/Tutorial/LibraryExamples/FtpClient 
3. SDライブラリについての詳細情報: https://www.arduino.cc/en/Reference/SD