---
title:                "文字列の連結"
html_title:           "Arduino: 文字列の連結"
simple_title:         "文字列の連結"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## 何となぜ？

文字列の連結とは、2つ以上の文字列を一つにまとめることを指します。プログラマーがこれを行う理由は、主にデータを整形したり、ユーザー向けのメッセージを作成したりするためです。

## どうするのか？

以下にコード例と出力例を示します。

```Arduino
String str1 = "Hello, ";
String str2 = "World!";
String str3 = str1 + str2; 
Serial.println(str3);  
```

これを実行すると以下のような結果が出力されます。

```Arduino
Hello, World!
```

## 詳細情報

文字列連結の概念は古くから存在しており、初期のプログラミング言語でも使用されていました。文字列連結の代替手段としては、sprintf関数やstrcat関数が提供されていますが、これらはC言語の関数であり、使い方が複雑でエラーが起きやすいという欠点があります。Arduinoでは、+演算子がオーバーロードされて文字列の連結に使われています。

## 参考情報

以下のリンクでは、関連する情報を見つけることができます。

- +演算子について：[Arduino Operators](https://www.arduino.cc/reference/en/language/structure/arithmetic-operators/addition/)

以上の内容が、Stringの連結方法についてあなたの理解を深める手助けになれば幸いです。