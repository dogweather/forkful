---
title:                "文字列の補間"
aliases:
- /ja/arduino/interpolating-a-string/
date:                  2024-01-20T17:50:11.861299-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列の補間"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/interpolating-a-string.md"
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
