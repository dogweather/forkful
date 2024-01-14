---
title:    "Arduino: 「ランダムな数字を生成する」"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## なぜ

プログラムでランダムな数字を生成することは、面白いプロジェクトやゲーム制作において重要な要素です。また、デバイスの動作をランダム化することで、センサーの正確性をテストすることができます。さまざまな用途でランダムな数字を生成する方法を見ていきましょう。

## 方法

ランダムな数字を生成するには、```random()```関数を使用します。この関数は、0から指定した最大値(引数)までの範囲内でランダムな数字を生成します。例えば、```random(10)```のように使用すれば、0から9までの範囲でランダムな数字を生成できます。

```Arduino
int randomNum = random(10);  // 0から9までの数字を生成
Serial.println(randomNum);  // 生成された数字をシリアルモニターに表示
```

また、この関数を複数回使用して、さらにランダムな数字を生成することもできます。例えば、```random(10) + random(10)```とすることで、0から18までの範囲内でランダムな数字を生成できます。

さらに、ランダムな数字を生成する際に、毎回同じ値が出力されるのを防ぐためには、```randomSeed()```関数を使用します。この関数には、任意の数字を引数として渡し、その数字を元に乱数のシードを生成することで、よりランダムな数字を生成できます。

```Arduino
randomSeed(analogRead(A0));  // A0ピンから乱数のシードを生成
int randomNum = random(10);  // 0から9までの数字を生成
Serial.println(randomNum);  // 生成された数字をシリアルモニターに表示
```

## ディープダイブ

ランダムな数字を生成する方法には、他にも様々な種類があります。例えば、乱数ジェネレーターを使用する方法や、ランダムな文字列を生成する方法などがあります。また、ランダムな数字を使用して、ゲームやアート作品を制作することもできます。

さらに、ランダムな数字を生成する際には、ハードウェア的な偶然性を利用することもできます。例えば、センサーの値やマイクロ秒などを使用することで、よりランダムな値を生成できます。

## それを見る

- [Arduino Reference - random()](https://www.arduino.cc/reference/en/language/functions/random-numbers/random/)
- [Arduino Reference - randomSeed()](https://www.arduino.cc/reference/en/language/functions/random-numbers/randomseed/)
- [Random.org](https://www.random.org/)