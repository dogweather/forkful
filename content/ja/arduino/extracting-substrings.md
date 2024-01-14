---
title:                "Arduino: サブストリング抽出"
simple_title:         "サブストリング抽出"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## なぜ
なぜ文字列から部分文字列を抽出するか。Arduinoプログラマーの皆さんは、時には文字列を処理しなければならない場面に遭遇することがあります。例えば、文字列から特定の情報を抽出したり、ある部分を置き換えたりする必要があるかもしれません。そのような場合には、文字列内の部分的な情報を取得するために部分文字列抽出が必要になる場合があります。

## テクニック
部分文字列抽出のプログラミング方法を説明します。まず、文字列を格納するための変数を定義します。次に、その変数から特定の位置にある文字を抽出するために、インデックスと呼ばれる数字を使用します。インデックスは、文字列内の文字の位置を指定するために使用されます。例えば、 "Hello"という文字列において、インデックス0は"H"、インデックス1は"e"を指定します。そして、抽出したい部分文字列の開始位置と終了位置をインデックスで指定します。抽出したい部分文字列を指定するためには、Arduinoのサブストリング関数を使用します。

```Arduino
// 文字列を格納する変数の定義
String str = "Hello World";

// 部分文字列を抽出する
String sub = str.substring(6, 11); // "World"が抽出される

// 抽出した部分文字列をシリアルモニターに出力する
Serial.println(sub); // "World"が出力される
```

## 詳細
部分文字列抽出には、さまざまな方法があります。上記の例では、特定の位置から特定の文字数を抽出する方法を見てきましたが、他にもさまざまな方法があります。例えば、部分文字列内の特定の文字を検索して抽出する方法や、正規表現を使用してパターンに一致する部分文字列を抽出する方法などがあります。Arduinoのドキュメントやインターネット上のチュートリアルを参考にして、自分に合った抽出方法を見つけてみてください。

## さらに見る
今回紹介したのは部分文字列抽出の基本的な方法でしたが、Arduinoでは他にも便利な文字列操作の関数がたくさんあります。以下のリンクを参考にして、さまざまな文字列操作の方法を学んでみましょう。

- https://www.arduino.cc/en/Reference/StringObject
- https://www.arduino.cc/en/Tutorial/StringConstructors
- https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/substring/