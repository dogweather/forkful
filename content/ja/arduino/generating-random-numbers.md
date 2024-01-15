---
title:                "ランダムな数字の生成"
html_title:           "Arduino: ランダムな数字の生成"
simple_title:         "ランダムな数字の生成"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## なぜ
ランダムな数値を生成することに興味がある人々は、ゲームやシミュレーション、セキュリティなどの分野で使用できるためです。Arduinoを使用したランダムな数値の生成は、そのような場面で役に立ちます。

## How To
ランダムな数値を生成するには、`random()`関数を使用します。この関数は、引数として最小値と最大値を受け取り、その範囲内でランダムな数値を返します。以下は、0から10までのランダムな数値を生成するArduinoコードの例です。

```Arduino
int randomNumber = random(0, 10); // 0から10までのランダムな数値を生成
Serial.println(randomNumber); // ランダムな数値をシリアルモニターに表示
```

このように、`random()`関数を使用することで簡単にランダムな数値を生成することができます。

## Deep Dive
Arduinoには、様々なランダムな数値の生成方法があります。`random()`関数だけでなく、`randomSeed()`関数を使用して最初のシード値を決めることで、より本格的なランダム性を実現することができます。

また、マイクロコントローラーのランダムな起動や使用時間などを使用して、よりシード値をランダム化することもできます。さらに、Arduinoには乱数を生成するためのライブラリもあり、様々な方法でランダムな数値を生成することができます。

## See Also
- [Arduino公式サイト](https://www.arduino.cc/)
- [Arduino開発環境のダウンロード](https://www.arduino.cc/en/software) 
- [Arduinoプロジェクト集光器](https://create.arduino.cc/projecthub)

以上で、ランダムな数値の生成についての基本的な情報を説明しました。これらの方法を使用して、さまざまなプロジェクトで有益なランダム性を実現してください。