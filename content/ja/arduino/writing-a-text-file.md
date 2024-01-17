---
title:                "テキストファイルの書き方"
html_title:           "Arduino: テキストファイルの書き方"
simple_title:         "テキストファイルの書き方"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 何 & なぜ？

テキストファイルを書くとは、テキストを特定の形式でコンピューターに保存することです。プログラマーはこれをすることで、データを簡単に取り出したり、他のアプリケーションで使用したりすることができます。

## やり方：

```Arduino
File myFile = SD.open("test.txt", FILE_WRITE);
if (myFile) {
  myFile.println("Hello World");
  myFile.close();
  Serial.println("File written successfully.");
} else {
  Serial.println("Error opening file.");
}
```
```
File written successfully.
```

## 深堀り：

- テキストファイルの書き込みは、SDカードなどの外部デバイスにも可能です。
- 代替の方法として、EEPROMやフラッシュメモリーにデータを保存することもできます。
- プログラムが再起動してもデータが失われないようにするためには、バッテリーバックアップされたリアルタイムクロックを使用することができます。

## 関連情報：

- [Arduino SDライブラリリファレンス](https://www.arduino.cc/en/Reference/SD)
- [SDライブラリの使用例](https://www.arduino.cc/en/Tutorial/LibraryExamples/SDReadWrite)
- [データの保存方法について詳しくはこちら](https://www.arduino.cc/en/Tutorial/Fat16Format)