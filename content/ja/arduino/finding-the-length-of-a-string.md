---
title:                "Arduino: 文字列の長さを見つける"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## なぜ

文字列の長さを求めることの重要性について説明します。このプログラミングスキルを習得することで、Arduinoプロジェクトにおいて文字列を効率的に処理することができます。

## 方法

文字列の長さを求めるには、以下のような「```Arduino ... ```」コードブロックのサンプルを参考にすることができます。

```Arduino
// 文字列の長さを求める関数を定義する
int getLength(String str) {
  // 文字列の1文字ずつカウントする
  int count = 0;
  for (int i = 0; i < str.length(); i++) {
    count++;
  }
  // 最後にカウントした数を返す
  return count;
}

// 文字列を定義する
String myString = "こんにちは";

// getLength()関数を呼び出して、文字列の長さを求める
int length = getLength(myString);

// シリアルモニターに文字列の長さを出力する
Serial.print("文字列の長さは： ");
Serial.println(length);
```

上記のサンプルコードでは、文字列の長さを求めるために、`String`オブジェクトの`length()`関数を使用しています。また、`for`ループを使用して文字列の長さをカウントしています。最後に、`getLength()`関数を定義し、文字列の長さを返すように設定しています。

## ディープダイブ

文字列の長さを求めるプロセスについてさらに詳しく説明します。

Arduinoでは、文字列は`String`オブジェクトとして処理されます。そのため、文字列の長さを`length()`関数を使用して取得することができます。しかし、文字列の長さを求めるには、`for`ループなどのループ処理によって、文字列の1文字ずつをカウントする必要があります。そのため、プログラミングの基礎知識が必要になります。

## 関連リンク

- [Wiring Strings - Arduino公式ドキュメント](https://www.arduino.cc/en/Tutorial/StringLengthTrim)
- [Stringオブジェクト - Arduino公式リファレンス](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [プログラミングの基礎知識 - Progate](https://prog-8.com/contents/basic/programming)