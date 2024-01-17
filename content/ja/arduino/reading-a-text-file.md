---
title:                "テキストファイルの読み込み"
html_title:           "Arduino: テキストファイルの読み込み"
simple_title:         "テキストファイルの読み込み"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 何？なぜ？
テキストファイルを読み込むことは、コンピューターがデータを読み取る方法の一つです。プログラマーは、プログラムで使用するデータをテキストファイルに保存し、それを読み込んで処理することで、手間を省くことができます。

## 方法：
```
Arduinoの記事
file = SD.open("test.txt", FILE_READ);
if (file) {
  Serial.println(file.read());
  file.close();
}
```

## もっと深く掘り下げる：
テキストファイルの読み込みは、古くからある基本的なプログラミング手法です。他にも、バイナリファイルを使用する方法もありますが、テキストファイルは人が理解しやすく、プログラムの変更も簡単にできます。Arduinoでは、SDライブラリを使用することで、SDカードに保存されたテキストファイルを読み込むことができます。

## 関連情報：
- [SDライブラリリファレンス - Arduino](https://www.arduino.cc/en/Reference/SD)
- [C言語ファイル入出力操作まとめ - Qiita](https://qiita.com/tajima_taso/items/9ebfd6d7574f8321dfb7)