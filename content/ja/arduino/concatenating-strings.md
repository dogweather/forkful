---
title:    "Arduino: 文字列の連結"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

# なぜ、文字列をつなげるのか？

C++言語で使用される、文字列をつなげるとは、2つ以上の文字列を合わせて1つの新しい文字列を作ることです。文字列をつなげることで、複数のデータを統合してより複雑なプログラムを作ることができます。また、Arduinoでは、文字列をつなげることでシリアルモニターにデータを表示させたり、データの送受信にも使用することができます。

# つなげ方の手順

Arduinoでは、文字列をつなげるために連結演算子「+」を使用します。下記のような例で、2つの文字列をつなげてシリアルモニターに表示させてみましょう。

```Arduino
String str1 = "Hello";
String str2 = "World";

String result = str1 + str2; 
// Hello Worldという文字列が結果として表示される
Serial.println(result);
```

このように、連結演算子を使用することで複数の文字列を簡単につなげることができます。また、文字列の前後に数字や変数を組み合わせることも可能です。下記の例では、「Hello1」や「Hello2」のように、変数と文字列を結合させて表示しています。

```Arduino
String str = "Hello";
int num = 1;

String result1 = str + num; 
// Hello1という文字列が結果として表示される
Serial.println(result1);

num++; // numの値を1増やして2にする
String result2 = str + num; 
// Hello2という文字列が結果として表示される
Serial.println(result2);
```

# 深く掘り下げる

Arduinoでは、Stringクラスに用意された関数を使用することで、より高度な文字列の操作ができます。例えば、substring()関数を使用することで、ある範囲にある文字列を取り出したり、indexOf()関数を使用することで、特定の文字列が含まれる位置を取得したりすることができます。

また、文字列の操作にはメモリの使用量にも注意する必要があります。文字列をつなげるたびに、新しいメモリ領域が必要となり、メモリの枯渇やプログラムの動作が遅くなる可能性があります。そのため、必要以上に文字列をつなげることは避けるようにしましょう。

# 今後も参考にしてほしいリンク

- Stringクラスの詳細: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- 文字列のつなげ方について詳しく学ぶ: https://www.arduino.cc/en/Tutorial/StringConcatenation
- Stringクラスの関数一覧: https://www.arduino.cc/reference/en/language/functions/communication/serial/println/