---
title:    "Arduino: パターンに一致する文字を削除する"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# なぜ
Arduinoを使ったプログラミングでは、文字列を操作することがよくあります。その中でも特に、指定したパターンに合致する文字を削除する方法は非常に便利です。

## 方法
文字列を操作するためには、まずは`String`オブジェクトを作成する必要があります。例として、次のように文字列を定義してみましょう。

```
Arduinoソースコード
String str = "abbbbccc";
```

次に、定義した`String`オブジェクトを使用して、`replace()`関数を使います。この関数は、指定したパターンに合致する文字を、指定した文字列に置き換えることができます。例えば、次のように書くことで`b`という文字を全て`a`に置き換えることができます。

```
Arduinoソースコード
str.replace("b", "a");
```

そして、最後に変更した文字列を出力するために、`Serial.println()`関数を使います。

```
Arduinoソースコード
Serial.println(str);
```

このコードを実行すると、次のような出力が得られます。

```
Arduinoソースコード
aacc
```

## 深く掘り下げる
`replace()`関数は、指定したパターンに合致する全ての文字を置き換えます。しかし、万が一置き換えたくない文字がある場合はどうでしょうか。その場合は、第3引数に置き換えない文字の個数を指定することで対応することができます。また、`replace()`関数の内部で`indexOf()`関数を使用しているため、置き換えるパターンにはワイルドカード(`*`)を使用することもできます。詳しくは公式ドキュメントを参照してみてください。

# 参考資料
- [String オブジェクト - Arduino 公式ドキュメント](https://www.arduino.cc/reference/ja/language/variables/data-types/stringobject/)
- [replace() - Arduino 公式ドキュメント](https://www.arduino.cc/reference/ja/language/variables/data-types/stringobject/replace/)
- [indexOf() - Arduino 公式ドキュメント](https://www.arduino.cc/reference/ja/language/variables/data-types/stringobject/indexof/)