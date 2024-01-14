---
title:                "Arduino: 文字列の連結"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

Arduinoプログラミングブログ

## なぜ

文字列の連結を行う理由について1〜2文で説明します。

コードを書く上で文字列の連結が必要な場合があります。例えば、センサーから取得した数値を文字列として表示する場合や、複数の文字列を組み合わせてメッセージを作成する場合などです。文字列の連結は、より柔軟なプログラムを作成するために欠かせない機能です。

## 方法

まず、連結したい文字列を ```Arduino``` プログラム内で宣言します。例えば、```String str1 = "Hello"```、```String str2 = "world!"```のようにです。そして、連結したい文字列を一つの変数に代入することで、文字列の連結が行えます。例えば、```String str3 = str1 + str2```のようにします。

また、文字列を結合する際には、文字列の間に```+```を用いて連結することができます。例えば、```String str1 = "Hello " + "world!"```のようにすることで、二つの文字列を一つの変数に代入することなく連結できます。

連結した文字列は```Serial.println()```を用いて出力することができます。例えば、 ```Serial.println(str3)```のようにすることで、```Hello world!```というメッセージがシリアルモニターに表示されます。

## 深堀り

文字列の連結を行う際には、メモリ管理に注意する必要があります。連結した文字列を一つの変数に代入すると、新しいメモリが割り当てられ、古い文字列は自動的に削除されます。しかし、頻繁に文字列の連結を行うとメモリの使用量が増え、プログラムのパフォーマンスに影響を与える可能性があります。そのため、必要以上に文字列の連結を行わないように注意しましょう。

## 参考

[Arduino Reference - Strings](https://www.arduino.cc/reference/en/language/variables/data-types/string/)

[GeeksforGeeks - Concatenation of two strings in Arduino](https://www.geeksforgeeks.org/concatenation-of-two-strings-in-arduino/)

[Arduinoプログラミングの基本 - Arduinoの文字列の操作](http://www.pangaea.co.jp/arduino/basic19.html)

# もっと詳しく

Arduinoの文字列操作について詳しく知りたい方は、以下のリンクを参考にしてください。

[Arduino Reference - StringsObjクラス](https://www.arduino.cc/reference/jp/language/variables/data-types/stringobject/)

[Arduino - Strig クラス](https://www.arduino.cc/en/Reference/String)

[スケッチファイルからリソースにアクセスする](http://www.pangaea.co.jp/arduino/reference/libraries/BINARYFILES/index.html)

## 参考

[Embedding Strings in Your Arduino Sketch](https://www.allaboutcircuits.com/technical-articles/embedding-strings-in-arduino-sketches/)