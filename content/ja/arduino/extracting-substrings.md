---
title:    "Arduino: 部分文字列の抽出"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## なぜ

ArduinoプログラミングにおけるSubstring（部分文字列）の抽出について、大型プロジェクトや文字列処理において便利なツールとして紹介します。

## 方法

Substringの抽出には、Stringクラスの`substring()`メソッドを使用します。まず、抽出したい文字列を定義し、その後に使用する`substring()`メソッドに引数として開始位置と終了位置のインデックスを指定します。以下の例では、「Hello world!」の「worl」を抽出しています。

```Arduino
String str = "Hello world!";
String extracted = str.substring(6, 10);
Serial.println(extracted);
// Output: world
```

部分文字列の開始位置と終了位置のインデックスは、インデックス0から始まることに注意してください。また、終了位置のインデックスは抽出したい文字列の最後の文字の次のインデックスを指定する必要があります。

## ディープダイブ

`substring()`メソッドを使用することで、文字列の中から任意の部分を抽出して利用することができます。また、引数に負の数を指定することで、文字列の末尾からの位置を指定することも可能です。例えば、以下のように末尾から3文字目から末尾までの部分文字列を抽出することができます。

```Arduino
String str = "Hello world!";
String extracted = str.substring(-3);
Serial.println(extracted);
// Output: ld!
```

さらに、`substring()`メソッドには第3引数として、抽出する部分文字列の長さを指定することもできます。例えば、以下のように抽出したい文字列の長さを6文字に指定することで、「world!」を抽出することができます。

```Arduino
String str = "Hello world!";
String extracted = str.substring(6, 12, 6);
Serial.println(extracted);
// Output: world!
```

## その他

上記の例では、抽出した部分文字列を別の変数に代入していますが、`substring()`メソッドは指定したインデックスの部分文字列を戻り値として返すため、直接出力することもできます。また、複数の`substring()`メソッドをつなげることで、複数の部分文字列を抽出することも可能です。

## 参考

[Arduino Stringクラス - substring()](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/substring/)