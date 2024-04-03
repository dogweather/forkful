---
date: 2024-01-20 17:50:11.861299-07:00
description: "How to (\u65B9\u6CD5): ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.475094-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u6587\u5B57\u5217\u306E\u88DC\u9593"
weight: 8
---

## How to (方法):
```Arduino
String temperature = "25";
String humidity = "80";

// 文字列を '+' で結合して補間する
String message = "Temperature: " + temperature + "C, Humidity: " + humidity + "%";
Serial.begin(9600);
Serial.println(message);

// 出力: Temperature: 25C, Humidity: 80%
```

## Deep Dive (深掘り):
インターポレーションを随所で見かけますが、Arduino では '+' 演算子を用いた文字列の結合で代わりに行います。C や C++ では通常、sprintf や std::ostringstream を使用することが可能ですが、これらはArduinoではメモリ管理の観点から避けられることが多いです。しかし、より効率的な方法やメモリに優しい手法が求められる場合があります。例えば、`String.reserve()`を使って、メモリの動的な割当てを減らすことができます。また、プログラムのサイズやパフォーマンスに影響が少ない`snprintf()`を利用する方法もあります。

## See Also (関連情報):
- Arduino の公式 String リファレンス: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- AVR libc におけるメモリ効率の良い sprintf: https://www.nongnu.org/avr-libc/user-manual/group__avr__stdio.html
