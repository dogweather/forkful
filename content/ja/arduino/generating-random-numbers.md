---
title:                "ランダム数の生成"
date:                  2024-01-20T17:48:31.756485-07:00
model:                 gpt-4-1106-preview
simple_title:         "ランダム数の生成"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (なぜか？)
ランダムな数を生成するとは、予測不可能な数字のシリーズを作ることです。これは、ゲームやセキュリティーシステム、シミュレーションなど多様なプログラムで必要とされます。

## How to: (やり方)
```Arduino
void setup() {
  Serial.begin(9600);
  randomSeed(analogRead(0));
}

void loop() {
  int randomNumber = random(100); // 0から99までのランダムな数
  Serial.println(randomNumber);
  delay(1000); // 次の数を生成する前に1秒待つ
}
```
サンプル出力:
```
23
79
4
...
```

## Deep Dive (深堀り)
- **歴史**: 数世紀にわたり乱数は数学、統計学で一役買ってきた。最初の乱数表は1901年に作られた。
- **代替**: ハードウェアRNG(乱数生成器)、ソフトウェアアルゴリズム（例：Mersenne Twister）など。
- **実装**: Arduinoの`randomSeed()`関数は乱数発生器を初期化するのに使う。`analogRead(0)`からのアナログ値を利用すると良い。`random(min, max)`は範囲指定のランダム数を生成する。

## See Also (関連情報)
- Arduino Reference for `random()`- https://www.arduino.cc/reference/en/language/functions/random-numbers/random/
- Arduino Reference for `randomSeed()` - https://www.arduino.cc/reference/en/language/functions/random-numbers/randomseed/
- Mersenne Twister algorithm - https://en.wikipedia.org/wiki/Mersenne_Twister