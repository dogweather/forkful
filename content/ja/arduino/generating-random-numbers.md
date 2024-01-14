---
title:                "Arduino: ランダム数字を生成する"
simple_title:         "ランダム数字を生成する"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## なぜ: ランダムな数字を生成する意義

ランダムな数字を生成することには多くの用途があります。例えば、ゲームやくじ引きのためのランダムなイベントを作り出すことができます。また、認証や暗号化のプログラムでセキュリティーを強化することにも役立ちます。

## やり方: Arduinoプログラミングの例とコードブロックの出力

Arduinoプログラムでランダムな数字を生成するには、 `random()` 関数を使用します。以下に例を示します。

```Arduino
// ランダムな数字を生成する
int randomNumber = random(10); // 0から9までの数字をランダムに生成
```

上記の例では、`random()` 関数に引数として生成したい数字の最大値を渡しています。また、Seed値を使用することで毎回同じランダムな数字を生成することができます。

```Arduino
// ランダムな数字を生成する
int randomNumber = random(10, 20); // 10から19までの数字をランダムに生成
```

`random()` 関数を使用することで、簡単にランダムな数字を生成することができます。

## 深堀り: ランダムな数字の生成について

Arduinoでは、 `random()` 関数を使用することで、擬似乱数を生成しています。つまり、Seed値によって生成された毎回同じ数列を元に、アルゴリズムにより次の数字を生成しています。そのため、Seed値を変えることで、異なる数列を生成することができます。

また、Arduinoでは擬似乱数を生成する標準関数以外にも、真の乱数を使用することができる外部ボードを接続することで、より高度なランダム性を持った数字を生成することができます。

## 参考リンク

- [Arduino公式ドキュメンテーション - 非決定的乱数サンプリング](https://www.arduino.cc/reference/en/language/functions/random-numbers/random/)
- [Arduinoプロジェクトでランダムなボイスを再生する方法](https://code-boxx.com/play-random-voice-arduino/)
- [Arduinoを使って擬似乱数を生成する方法](https://roothoops.com/arduino-random-number/)
- [Arduinoと乱数生成器の比較](https://www.youtube.com/watch?v=bnz8opMh830)

## 参考になるリンク

- [Arduino チュートリアル](https://www.arduino.cc/en/Tutorial/HomePage)
- [プログラミング入門編](https://qiita.com/tochiji/items/0e0a1e0a566353b1cf72)
- [コーディング力を向上するための練習問題サイト](https://yukicoder.me/)