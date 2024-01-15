---
title:                "新しいプロジェクトを始める"
html_title:           "Arduino: 新しいプロジェクトを始める"
simple_title:         "新しいプロジェクトを始める"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/starting-a-new-project.md"
---

{{< edit_this_page >}}

## なぜ
Arduinoを使って新しいプロジェクトを始める理由は様々あります。Arduinoは初心者でも簡単に使うことができ、様々なセンサーやモジュールとの組み合わせで自分だけのユニークなプロジェクトを作ることができます。

## 作り方
Arduinoのプログラミングは基本的には「setup」と「loop」の2つの関数で構成されています。まずはセットアップ関数に必要な初期設定を記述し、その後にループ関数の中に処理を書いていきます。以下に例を示します。

```Arduino
// LEDを点滅させるプログラム

// ピン番号の定義
int LED_PIN = 13;

// setup関数の定義
void setup() {
  // ピンを出力モードに設定
  pinMode(LED_PIN, OUTPUT);
}

// loop関数の定義
void loop() {
  // LEDを点灯
  digitalWrite(LED_PIN, HIGH);
  // 1秒待つ
  delay(1000);
  // LEDを消灯
  digitalWrite(LED_PIN, LOW);
  // 1秒待つ
  delay(1000);
}
```

上記のプログラムでは、13番ピンに接続されたLEDを1秒ごとに点滅させることができます。このように簡単なコードでも様々な操作が可能なので、ぜひチャレンジしてみてください！

## 詳細
新しいプロジェクトを始める際には、まずは自分がやりたいことを明確にすることが重要です。具体的に何を作りたいのか、どのようなセンサーやモジュールが必要なのか、どのようなコードが必要なのかを考えましょう。

また、Arduinoは豊富なライブラリがあるので、自分でコードを書く前にまずはライブラリを探してみることもおすすめです。既に完成されたコードを活用することで、より短時間でプロジェクトを完成させることができます。

## 他にも見るべき
- [Arduino 公式サイト](https://www.arduino.cc/)
- [Arduino プログラミング入門](https://chirimen.org/arduino/intro/)
- [Arduinoチュートリアル - 私の備忘録](https://keita-i.hatenablog.com/entry/20191102/1572671511)