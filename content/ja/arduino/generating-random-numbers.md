---
title:                "Arduino: ランダムな数字の生成"
programming_language: "Arduino"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## なぜ乱数を生成するのか？

乱数は、Arduinoプログラミングの多くのアプリケーションで重要な役割を果たします。例えば、サイコロやカードゲーム、ランダムな音楽の生成などに使用されます。また、セキュリティの観点からも乱数の生成は重要です。Arduinoで乱数を使うことで、多様な機能を実現することができます。

## 乱数の生成方法

乱数を生成するには、Arduinoの「random()」関数を使います。この関数は、乱数を生成するための擬似乱数ジェネレーターを内蔵しています。以下のコードを参考にしてください。

```Arduino
// 0から100までの乱数を生成する
int randomNumber = random(0, 100);
```

このコードを実行すると、0から100の間のランダムな数値が取得できます。また、乱数を生成する前に「randomSeed()」関数を使用することで、よりランダムな数値を生成することができます。以下のコードを参考にしてください。

```Arduino
// ランダムな初期値を使用して乱数を生成する
randomSeed(analogRead(A0));
```

## ディープダイブ

Arduinoの「random()」関数は、内蔵の擬似乱数ジェネレーターのシード値を基にして乱数を生成します。これは、Arduinoの起動時に決められた初期値を使用するため、同じプログラムを実行しても同じ結果が得られる可能性があります。そのため、乱数を使用する前に「randomSeed()」関数を使用することで、よりランダムな数値を得ることができます。

また、Arduinoの擬似乱数ジェネレーターは、実際の乱数ではなく、予測可能な数列を生成します。そのため、セキュリティ目的で乱数を使用する場合は、別の方法を検討する必要があります。

## 参考リンク

- Arduino公式ドキュメント：[Random Numbers](https://www.arduino.cc/reference/en/language/functions/random-numbers/)
- Arduinoチュートリアル：[Generating Random Numbers with Arduino](https://create.arduino.cc/projecthub/Arduino_Genuino/generating-random-numbers-with-arduino-5b677a)
- 乱数ジェネレーターに関する詳細な説明：[Pseudorandom Number Generator](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)