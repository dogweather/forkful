---
date: 2024-01-20 18:02:41.124500-07:00
description: "How to: (\u65B9\u6CD5\uFF1A) ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.498943-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u65B0\u3057\u3044\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\u3092\u59CB\u3081\u308B"
weight: 1
---

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
