---
aliases:
- /ja/arduino/interpolating-a-string/
date: 2024-01-20 17:50:11.861299-07:00
description: "\u6587\u5B57\u5217\u88DC\u9593\u306F\u3001\u5909\u6570\u3084\u5F0F\u306E\
  \u7D50\u679C\u3092\u6587\u5B57\u5217\u306E\u4E2D\u306B\u76F4\u63A5\u57CB\u3081\u8FBC\
  \u3080\u3053\u3068\u3067\u3059\u3002\u30B3\u30FC\u30C9\u3092\u8AAD\u307F\u3084\u3059\
  \u304F\u3001\u30E1\u30C3\u30BB\u30FC\u30B8\u3092\u52D5\u7684\u306B\u4F5C\u6210\u3059\
  \u308B\u305F\u3081\u306B\u4F7F\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:55.140309
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u88DC\u9593\u306F\u3001\u5909\u6570\u3084\u5F0F\u306E\
  \u7D50\u679C\u3092\u6587\u5B57\u5217\u306E\u4E2D\u306B\u76F4\u63A5\u57CB\u3081\u8FBC\
  \u3080\u3053\u3068\u3067\u3059\u3002\u30B3\u30FC\u30C9\u3092\u8AAD\u307F\u3084\u3059\
  \u304F\u3001\u30E1\u30C3\u30BB\u30FC\u30B8\u3092\u52D5\u7684\u306B\u4F5C\u6210\u3059\
  \u308B\u305F\u3081\u306B\u4F7F\u3044\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u306E\u88DC\u9593"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)

文字列補間は、変数や式の結果を文字列の中に直接埋め込むことです。コードを読みやすく、メッセージを動的に作成するために使います。

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
