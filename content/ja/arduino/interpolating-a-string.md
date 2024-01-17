---
title:                "文字列の補間"
html_title:           "Arduino: 文字列の補間"
simple_title:         "文字列の補間"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 何＆なんでや？
文字列を補完するとは、変数や計算値を含めた文字列を作ることです。プログラマーは、この方法を使うことで、動的に変化するテキストを生成することができ、効率的なコードを書くことができます。

## 使い方：
```Arduino
String name = "太郎";
int age = 25;
String message = "こんにちは、私の名前は" + name + "です。" + age + "歳です。";
Serial.println(message);
```

出力は「こんにちは、私の名前は太郎です。25歳です。」となります。

## さらに深く：
文字列の補完は、動的に変化するテキストを生成するのに便利な方法です。これは基本的に、プログラマーが文字列を静的に書くのではなく、変数や計算値を含めたものを作ることができるということです。これにより、同じコードを使って様々なパターンの文字列を生成することができます。代替手段として、文字列を連結する方法もありますが、補完を使うことでより簡単に可読性の高いコードを書くことができます。

Arduinoでは、```String```オブジェクトの```+```演算子を用いて、文字列の補完を行います。基本的には、```+```演算子は数値の足し算を行うものですが、だんだん多機能化してきた言語では、文字列の結合もできるようになりました。

## 他にも見てみる：
補完に関する詳細な情報は、こちらの情報源をご覧ください。
- [Wikipedia - 文字列の補間](https://ja.wikipedia.org/wiki/%E6%96%87%E5%AD%97%E5%88%97%E3%81%AE%E8%A3%9C%E9%96%93)
- [Arduino 公式サイト - Stringsオブジェクト](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- [YouTube - 文字列の補完](https://www.youtube.com/watch?v=V5CIYK_VVAg)