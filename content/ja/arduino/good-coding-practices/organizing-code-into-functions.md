---
date: 2024-01-26 01:09:24.414714-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:42.504622-06:00'
model: gpt-4-1106-preview
summary: "\u2026"
title: "\u30B3\u30FC\u30C9\u3092\u95A2\u6570\u306B\u6574\u7406\u3059\u308B"
weight: 18
---

## 何となぜ？
コードを関数に編成するとは、あなたのコードを再利用可能なチャンクに分割することを意味します。それぞれのチャンクは特定の仕事を行います。プログラマーは、コードを読みやすく、デバッグしやすく、再利用しやすくするためにこれを行います。これは、何かを作りたいときに毎回カオスな山から探し出すことを避けるために、レゴブロックを箱に分類するようなものです。

## 方法：
LEDを点滅させたいと想像してください。関数を使わなければ、あなたの`loop`はごちゃごちゃの塊です。関数を使うことで、それは整然とします。こうやってみましょう：

```Arduino
const int LED_PIN = 13;

void setup() {
  pinMode(LED_PIN, OUTPUT);
}

void loop() {
  blinkLED(500); // LEDを500ms毎に点滅させる
}

// LEDを点滅させる関数
void blinkLED(int delayTime) {
  digitalWrite(LED_PIN, HIGH);
  delay(delayTime);
  digitalWrite(LED_PIN, LOW);
  delay(delayTime);
}
```

サンプル出力：あなたのLEDは楽しそうに点滅しており、コードの目的が一目で明確です。

## 深掘り
関数が登場する前、プログラミングはスタートからエンドまでごとにある道路旅行でした。関数が登場した後、それは飛行機に飛び乗るようなものになり、重要な部分に直接スキップします。歴史的には、サブルーチン（初期の関数）はプログラミングにおける革命であり、コーダーが自分自身を繰り返すことを避けることを可能にしました — それがDRY原則です、Don’t Repeat Yourself（自分自身を繰り返さない）。関数の代わりとなるものには、マクロやオブジェクト指向プログラミング（OOP）用のクラスの使用が含まれるかもしれません。肝心なこと？関数を定義するとき、コンパイラに対してタスクの実行のための設計図を与えています。Arduinoでは、しばしば単純なコマンドとして機能するvoid関数を定義しますが、関数は値を返すこともでき、より汎用性があります。

## 参照
関数についてもっと知るために、以下を参照してください：

- Arduinoの公式関数リファレンス：https://www.arduino.cc/reference/en/language/functions/
- DRY原則についてもっと学ぶ：https://en.wikipedia.org/wiki/Don%27t_repeat_yourself
- サブルーチンの歴史上のリフレッシャー：https://en.wikipedia.org/wiki/Subroutine
