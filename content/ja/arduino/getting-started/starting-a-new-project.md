---
date: 2024-01-20 18:02:41.124500-07:00
description: ''
isCJKLanguage: true
lastmod: '2024-04-05T22:50:56.387403-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5\uFF1A) \u65B0\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\u3092\u59CB\
  \u3081\u308B\u3068\u304D\u306F\u3001\u53E4\u5178\u7684\u306A\"Blink\"\u30B9\u30B1\
  \u30C3\u30C1\u304C\u57FA\u672C\u3002\u3053\u306E\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\
  \u306F\u3001Arduino\u306E\u6B74\u53F2\u3067\u521D\u5FC3\u8005\u5411\u3051\u306E\u5165\
  \u9580\u3068\u3055\u308C\u3066\u304A\u308A\u3001\u307B\u3068\u3093\u3069\u306E\u30C1\
  \u30E5\u30FC\u30C8\u30EA\u30A2\u30EB\u3067\u53D6\u308A\u4E0A\u3052\u3089\u308C\u3066\
  \u3044\u308B\u3002\u4EE3\u66FF\u3068\u3057\u3066\u306F\u3001LED\u4EE5\u5916\u306E\
  \u90E8\u54C1\u3092\u4F7F\u3063\u305F\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\u3082\u8003\
  \u3048\u3089\u308C\u308B\u304C\u3001\"Blink\"\u306F\u6C17\u8EFD\u306B\u59CB\u3081\
  \u3089\u308C\u308B\u305F\u3081\u975E\u5E38\u306B\u4EBA\u6C17\u304C\u3042\u308B\u3002\
  \u5177\u4F53\u7684\u306A\u5B9F\u88C5\u3067\u306F\u3001`pinMode()`\u306F\u4F7F\u3044\
  \u305F\u3044\u30D4\u30F3\u306E\u5165\u51FA\u529B\u30E2\u30FC\u30C9\u3092\u8A2D\u5B9A\
  \u3057\u3001`digitalWrite()`\u3067HIGH\u307E\u305F\u306FLOW\u306E\u4FE1\u53F7\u3092\
  \u30D4\u30F3\u306B\u9001\u3063\u3066\u30C7\u30D0\u30A4\u30B9\u3092\u5236\u5FA1\u3059\
  \u308B\u3002`delay()`\u95A2\u6570\u306F\u6307\u5B9A\u3057\u305F\u30DF\u30EA\u79D2\
  \u3060\u3051\u30D7\u30ED\u30B0\u30E9\u30E0\u3092\u505C\u6B62\u3055\u305B\u308B\u3002"
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
