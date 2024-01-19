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

## 何となぜ？ (What & Why?)
文字列の補間とは、文字列内に変数を組み込むことです。補間によって、コードの可読性が向上し、デバッグが容易になります。

## やり方 (How to)
Arduinoでは直接的な文字列補間の方法はありませんが、`String(variable)`関数の組み合わせで達成できます。下記に例を示します。

```Arduino
int num = 5;
String text = "The number is: ";
String message = text + String(num);
Serial.println(message);
```

このコードは以下のような出力になります。

```
The number is: 5
```

## ディープダイブ (Deep Dive)
文字列補間はプログラミングの古くから存在し、その可読性と便利性から広く使われています。Arduino言語では、PythonやJavaScriptなどと比べて直感的な文字列補間機能が欠けていますが、上記のような方法で補うことができます。

他の代替策としては、`sprintf()`関数を使う方法もあります。しかし、この関数はメモリーを多く消費し、Arduinoのようなリソースが限られている環境では推奨されません。

## 関連資料 (See Also)
- [Arduino Reference: String()](https://www.arduino.cc/reference/jp/language/variables/data-types/string/functions/string/)
- [Arduino Reference: printf()](https://www.arduino.cc/reference/jp/language/functions/communication/serial/printf/)
- [The Evils of Arduino Strings](https://hackingmajenkoblog.wordpress.com/2016/02/04/the-evils-of-arduino-strings/)