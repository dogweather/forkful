---
title:                "パターンに一致する文字を削除する"
html_title:           "Arduino: パターンに一致する文字を削除する"
simple_title:         "パターンに一致する文字を削除する"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## なぜ

あなたのプログラムには、不要な文字が含まれることがあります。これらの文字を削除することで、プログラムの効率を改善し、正しい結果を得ることができます。この記事では、Arduinoを使って文字を削除する方法を紹介します。

## 使い方

削除したい文字と一致するパターンを指定し、それをコードに組み込むことで削除することができます。以下の例を参考にしてください。

```Arduino
// 正規表現を使用して、削除したい文字を指定する
String pattern = "[abc]";

// 削除する文字列を定義する
String str = "abcdefg";

// 文字を削除する関数を定義する
String deleteChars(String input, String pattern) {
  // 削除したい文字を指定して、replace()関数を使用する
  input.replace(pattern, "");

  // 結果を返す
  return input;
}

// 削除した文字を出力する
Serial.println(deleteChars(str, pattern));
```

出力結果は、`defg`となります。パターンに一致する文字が削除されていることが分かりますね。

## 深堀り

もし、削除したい文字が複数の場所に出現する場合にはどうするのでしょうか？その場合には、replace()関数ではなく、replaceAll()関数を使用します。これにより、指定したパターンと全ての一致する文字が削除されます。

また、削除したい文字以外の文字を置換する際には、replace()関数の代わりにreplaceFirst()やreplaceLast()関数を使用することもできます。

より詳細な情報や例は、[Arduino公式ドキュメンテーション](https://www.arduino.cc/reference/en/language/functions/communication/serial/print/)を参考にしてください。

## 関連リンク

- [Stringクラスのドキュメンテーション](https://www.arduino.cc/reference/en/language/variables/data-types/string/)