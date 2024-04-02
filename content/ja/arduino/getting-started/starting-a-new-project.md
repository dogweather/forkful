---
date: 2024-01-20 18:02:41.124500-07:00
description: "\u65B0\u3057\u3044\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\u3092\u59CB\u3081\
  \u308B\u3063\u3066\u306E\u306F\u3001\u7A7A\u306E\u30B9\u30B1\u30C3\u30C1\u304B\u3089\
  \u65B0\u3057\u3044\u30A2\u30A4\u30C7\u30A2\u3092\u73FE\u5B9F\u306B\u3059\u308B\u3053\
  \u3068\u3060\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30AA\u30EA\u30B8\
  \u30CA\u30EA\u30C6\u30A3\u3092\u767A\u63EE\u3057\u3001\u554F\u984C\u3092\u89E3\u6C7A\
  \u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3046\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.498943-06:00'
model: gpt-4-1106-preview
summary: "\u65B0\u3057\u3044\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\u3092\u59CB\u3081\
  \u308B\u3063\u3066\u306E\u306F\u3001\u7A7A\u306E\u30B9\u30B1\u30C3\u30C1\u304B\u3089\
  \u65B0\u3057\u3044\u30A2\u30A4\u30C7\u30A2\u3092\u73FE\u5B9F\u306B\u3059\u308B\u3053\
  \u3068\u3060\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30AA\u30EA\u30B8\
  \u30CA\u30EA\u30C6\u30A3\u3092\u767A\u63EE\u3057\u3001\u554F\u984C\u3092\u89E3\u6C7A\
  \u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3046\u3002"
title: "\u65B0\u3057\u3044\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\u3092\u59CB\u3081\u308B"
weight: 1
---

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
