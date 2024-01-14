---
title:                "Arduino: 正規表現の使用"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## なぜ正規表現を使うのか

正規表現は、Arduinoプログラミングにおいて強力なツールです。データのパターンを特定し、処理するために使われます。例えば、センサーからのデータを解析する場合や、入力された文字列を検証する場合などに非常に便利です。

## 使い方

正規表現を使うには、まず `#include <regex>`という行をプログラムに追加する必要があります。次に、正規表現オブジェクトを作成し、パターンを指定します。例えば、`regex pattern("abc");`というように書くことができます。これは、"abc"という文字列にマッチするパターンを定義しています。

以下は、正規表現を使った簡単なコード例です。

```Arduino
#include <regex>

String input = "Hello World!";
regex pattern("o.*d");

if (regex_search(input, pattern)) {
  Serial.println("Match found!");
  // Output: "Match found!"
}
```

上記の例では、`input`という文字列が`o`で始まり`d`で終わるパターンにマッチするかどうかを検証しています。もしマッチすれば、`Match found!`というメッセージがシリアルモニターに出力されます。

## より深く掘り下げる

正規表現を使う際には、より詳細なパターンを指定することができます。例えば、`abc`という文字列にマッチするだけではなく、`ab`の後に任意の文字列が続き、最後に`c`があるパターンを指定したい場合は、`regex pattern("ab.*c");`というように書くことができます。

また、正規表現には様々なメタ文字があり、特定の文字の繰り返しを指定したり、特定の文字の範囲を指定することもできます。詳しくは、正規表現のチュートリアルを参考にしてください。

## 関連リンク

- [正規表現チュートリアル](https://www.geeksforgeeks.org/regular-expression-regex-in-c/)
- [ArduinoのStringクラス](https://www.arduino.cc/reference/ja/language/variables/data-types/stringobject/)