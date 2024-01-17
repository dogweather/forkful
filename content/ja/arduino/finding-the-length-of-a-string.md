---
title:                "文字列の長さを見つける"
html_title:           "Arduino: 文字列の長さを見つける"
simple_title:         "文字列の長さを見つける"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# What & Why?
 文字列の長さを求めるとは何か？プログラマーがそれを行う理由は？

文字列の長さを求めるとは、文字列の文字数をカウントすることです。プログラマーがこれを行う理由は、入力された文字列の長さに基づいてプログラムの処理を調整するためです。

# How to:
 Arduinoのプログラム例とサンプル出力を```Arduino ...```のコードブロックで示します。

```
Arduinoのプログラム例:
char str[] = "こんにちは";
int len = strlen(str);
Serial.print("文字列の長さ：");
Serial.println(len);

サンプル出力：
文字列の長さ：5
```

# Deep Dive:
 1. 文字列の長さを求めることは、1960年代に開発されたC言語の標準ライブラリ関数である「strlen」に由来しています。
 2. 他のプログラミング言語では、文字列の長さを求めるための組み込み関数が提供されています。例えば、Pythonでは「len()」関数を使います。
 3. Arduinoでは、文字列の長さを求めるために「strlen()」関数を使用する前に、string.hライブラリをインクルードする必要があります。
 
# See Also:
関連リソースへのリンク

- Arduinoリファレンス：https://www.arduino.cc/reference/ja/language/variables/data-types/string/functions/strlen/
- Pythonリファレンス：https://docs.python.org/ja/3/library/stdtypes.html#str.__len__