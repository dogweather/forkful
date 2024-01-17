---
title:                "ランダム数字を生成する"
html_title:           "Arduino: ランダム数字を生成する"
simple_title:         "ランダム数字を生成する"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 概要

Arduinoにおいて、ランダムな数値を生成することができます。プログラマーたちは、ランダムな数値を生成することで、アプリケーションやゲームの振る舞いをよりランダムにすることができます。

## 使い方

```Arduino
// 1から10までのランダムな数値を生成する例
int random_number = random(1, 10);
Serial.println(random_number); // 例：5
```

## 詳細を調べる

生成されるランダムな数値は、疑似乱数と呼ばれます。これは、厳密には真のランダム性を持たない数値ですが、十分なランダム性を発生することができます。

疑似乱数の生成には、様々な手法があります。Arduinoでは、乱数生成アルゴリズムの一つであるメルセンヌ・ツイスター法が使用されています。

代替手段として、ハードウェアによる真の乱数生成器を使用することもできます。しかし、このような方法は高価であるため、Arduinoではあまり使用されません。

ランダムな数値は、さまざまなアプリケーションで利用されています。例えば、ゲームの敵の出現、アニメーションの再生順序の決定などに使用されます。

## 関連リンク

- [Arduino Reference - random()](https://www.arduino.cc/reference/en/language/functions/random-numbers/random/)
- [Wikipedia - Pseudorandom number generator](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)
- [Wikipedia - Mersenne Twister](https://en.wikipedia.org/wiki/Mersenne_Twister)
- [SparkFun - Random Numbers](https://learn.sparkfun.com/tutorials/random-numbers)