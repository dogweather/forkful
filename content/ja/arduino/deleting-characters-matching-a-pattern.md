---
title:                "Arduino: パターンに一致する文字を削除する"
simple_title:         "パターンに一致する文字を削除する"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## なぜ

Arduinoプログラミングでは、テキストデータを処理する際に特定のパターンに一致する文字を削除する必要がある場合があります。これは、データの整理や統計処理を行う際に便利な方法です。

## 方法

Arduinoのプログラムで文字を削除する方法はいくつかあります。最も基本的な方法は、文字列を一つずつチェックし、パターンに一致する場合は空白文字または空の文字列に置き換えることです。以下の例を参考にしてください。

```Arduino
// テキストデータの文字列を定義 
String text = "Arduinoは楽しい!";

// 文字列を削除したいパターンを定義 
String pattern = "楽しい";

// 文字列からパターンに一致する文字を削除する 
text.replace(pattern, "");

// 出力 
Arduinoは!
```

複雑なパターンを扱う場合は、正規表現を使用することもできます。正規表現を使用すると、パターンに一致する文字を簡単に置き換えることができます。以下の例を参考にしてください。

```Arduino
// テキストデータの文字列を定義 
String text = "1234 5678 9101112";

// 文字列から数字のみを除外する 
text.replaceAll("[0-9]", "");

// 出力 
 
```

## 深堀り

Text.replace()やreplaceAll()は、ArduinoのStringオブジェクトが持つ便利なメソッドですが、文字列を一つずつ処理するため、大きなデータを処理する場合は効率が悪くなる可能性があります。そのため、より高速な処理が必要な場合は、C言語の標準ライブラリであるstring.hを使用する方法もあります。

例えば、Arduinoのプログラムで使用できるstrtok()関数は、指定したデリミタ文字に基づいて文字列を分割することができます。これを使用すると、文字列を一度に一部ずつ処理することができるため、より高速な削除処理が可能になります。

## おわりに

今回はArduinoプログラミングで文字を削除する方法について紹介しました。それぞれの方法には長所と短所がありますが、自分のプロジェクトに最適な方法を選択してみてください。

## 関連リンク

- [Stringオブジェクトのドキュメント](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- [正規表現のチュートリアル](https://www.w3schools.com/jsref/jsref_obj_regexp.asp)
- [string.hのドキュメント](https://www.tutorialspoint.com/c_standard_library/string_h.htm)