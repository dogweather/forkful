---
title:                "ランダムな数字の生成"
html_title:           "C#: ランダムな数字の生成"
simple_title:         "ランダムな数字の生成"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 何となぜ？

ランダムな数値生成とは、予測不可能な数値を生み出すプロセスです。プログラマーはこれを使って、例えばゲームの要素をランダム化したり、一意のIDを生成したりします。

## 使い方:

```Arduino
void setup() {
  // 初期化.
  Serial.begin(9600);
  // randomSeed関数の初期化.
  randomSeed(analogRead(0));
}

void loop() {
  // ランダムな整数を生成し、シリアルモニターに出力.
  Serial.println(random(100));
  delay(1000);  // 次の数値を生成する前の遅延.
}
```

上記のコードは、0から99までのランダムな整数を毎秒生成します。randomSeedは、異なるシーケンスを生成するためのランダム関数を初期化します。

## 深堀り

ランダムな数値生成は、初期のコンピューターシステムの時代から存在しています。これは、アプリケーションが予測不可能な要素や値を必要とするたびに利用されます。

Arduinoでは、`random()`と`randomSeed()`といった関数が提供されています。しかし、他の方法、例えば、ハードウェアからのノイズや、外部のエントロピーソースを利用する方法もあります。

また、Arduinoの`random()`関数は一部の範囲内での均等な分布を提供しないという限定的な事実があります。この点は特に重要な場合は、カスタムのランダム生成関数を使用することを考慮に入れてください。

## 参考情報

1. Arduino公式サイトでの[random関数](https://www.arduino.cc/reference/en/language/functions/random-numbers/random/) のドキュメンテーション。
2. [エントロピーソース](https://en.wikipedia.org/wiki/Entropy_(computing)) とは何かについての詳細情報。
3. 特定の範囲内で等確率のランダム数を生成するための[カスタム関数](https://forum.arduino.cc/index.php?topic=261053.0)の例。