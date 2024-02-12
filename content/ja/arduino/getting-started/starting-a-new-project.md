---
title:                "新しいプロジェクトを始める"
aliases:
- /ja/arduino/starting-a-new-project/
date:                  2024-01-20T18:02:41.124500-07:00
model:                 gpt-4-1106-preview
simple_title:         "新しいプロジェクトを始める"

tag:                  "Getting Started"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)

新しいプロジェクトを始めるってのは、空のスケッチから新しいアイデアを現実にすることだ。プログラマーは、オリジナリティを発揮し、問題を解決するためにこれを行う。

## How to: (方法：)

```Arduino
// 新プロジェクトのサンプルコード例
void setup() {
  // ピンモードをセットアップ
  pinMode(LED_BUILTIN, OUTPUT);
}

void loop() {
  // 内蔵LEDを点滅させる
  digitalWrite(LED_BUILTIN, HIGH);   
  delay(1000);                       
  digitalWrite(LED_BUILTIN, LOW);    
  delay(1000);                       
}
```

このコードは、内蔵LEDを1秒間隔で点滅させます。

## Deep Dive (深掘り)

新プロジェクトを始めるときは、古典的な"Blink"スケッチが基本。このプロジェクトは、Arduinoの歴史で初心者向けの入門とされており、ほとんどのチュートリアルで取り上げられている。代替としては、LED以外の部品を使ったプロジェクトも考えられるが、"Blink"は気軽に始められるため非常に人気がある。具体的な実装では、`pinMode()`は使いたいピンの入出力モードを設定し、`digitalWrite()`でHIGHまたはLOWの信号をピンに送ってデバイスを制御する。`delay()`関数は指定したミリ秒だけプログラムを停止させる。

## See Also (関連項目)

- [Arduino 公式サイト](https://www.arduino.cc/)
- [Arduino スタートガイド](https://www.arduino.cc/en/Guide/HomePage)
- [Arduino リファレンス](https://www.arduino.cc/reference/en/)
