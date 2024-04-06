---
date: 2024-01-20 17:50:11.861299-07:00
description: "How to (\u65B9\u6CD5): \u30A4\u30F3\u30BF\u30FC\u30DD\u30EC\u30FC\u30B7\
  \u30E7\u30F3\u3092\u968F\u6240\u3067\u898B\u304B\u3051\u307E\u3059\u304C\u3001Arduino\
  \ \u3067\u306F '+' \u6F14\u7B97\u5B50\u3092\u7528\u3044\u305F\u6587\u5B57\u5217\u306E\
  \u7D50\u5408\u3067\u4EE3\u308F\u308A\u306B\u884C\u3044\u307E\u3059\u3002C \u3084\
  \ C++ \u3067\u306F\u901A\u5E38\u3001sprintf \u3084 std::ostringstream\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.295313-06:00'
model: gpt-4-1106-preview
summary: "\u30A4\u30F3\u30BF\u30FC\u30DD\u30EC\u30FC\u30B7\u30E7\u30F3\u3092\u968F\
  \u6240\u3067\u898B\u304B\u3051\u307E\u3059\u304C\u3001Arduino \u3067\u306F '+' \u6F14\
  \u7B97\u5B50\u3092\u7528\u3044\u305F\u6587\u5B57\u5217\u306E\u7D50\u5408\u3067\u4EE3\
  \u308F\u308A\u306B\u884C\u3044\u307E\u3059\u3002C \u3084 C++ \u3067\u306F\u901A\u5E38\
  \u3001sprintf \u3084 std::ostringstream \u3092\u4F7F\u7528\u3059\u308B\u3053\u3068\
  \u304C\u53EF\u80FD\u3067\u3059\u304C\u3001\u3053\u308C\u3089\u306FArduino\u3067\u306F\
  \u30E1\u30E2\u30EA\u7BA1\u7406\u306E\u89B3\u70B9\u304B\u3089\u907F\u3051\u3089\u308C\
  \u308B\u3053\u3068\u304C\u591A\u3044\u3067\u3059\u3002\u3057\u304B\u3057\u3001\u3088\
  \u308A\u52B9\u7387\u7684\u306A\u65B9\u6CD5\u3084\u30E1\u30E2\u30EA\u306B\u512A\u3057\
  \u3044\u624B\u6CD5\u304C\u6C42\u3081\u3089\u308C\u308B\u5834\u5408\u304C\u3042\u308A\
  \u307E\u3059\u3002\u4F8B\u3048\u3070\u3001`String.reserve()`\u3092\u4F7F\u3063\u3066\
  \u3001\u30E1\u30E2\u30EA\u306E\u52D5\u7684\u306A\u5272\u5F53\u3066\u3092\u6E1B\u3089\
  \u3059\u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002\u307E\u305F\u3001\u30D7\u30ED\
  \u30B0\u30E9\u30E0\u306E\u30B5\u30A4\u30BA\u3084\u30D1\u30D5\u30A9\u30FC\u30DE\u30F3\
  \u30B9\u306B\u5F71\u97FF\u304C\u5C11\u306A\u3044`snprintf()`\u3092\u5229\u7528\u3059\
  \u308B\u65B9\u6CD5\u3082\u3042\u308A\u307E\u3059\u3002"
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
