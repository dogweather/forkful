---
date: 2024-01-20 17:33:57.554254-07:00
description: "How to: (\u3084\u308A\u65B9) ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.484787-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u6587\u5B57\u5217\u306E\u9023\u7D50"
weight: 3
---

## How to: (やり方)
```Arduino
String str1 = "こん";
String str2 = "にちは、";
String str3 = "Arduino!";
String combined = str1 + str2 + str3; // 文字列を連結

Serial.begin(9600);
Serial.println(combined); // 結果を出力: こんにちは、Arduino!
```

## Deep Dive (深い掘り下げ)
昔、Arduinoではメモリが少なかったため、文字列の連結には注意が必要でした。代替手段として`char`配列と関数`strcat()`が使われてきましたが、扱いにくいです。現在のArduinoでは`String`クラスを使用することで、簡単に文字列の連結ができます。ただし、背後でメモリの動的確保が行われるため、使いすぎるとメモリ断片化の問題が起こる可能性がまだあります。

## See Also (関連情報)
- [Arduino String Reference](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [Arduino Memory](https://www.arduino.cc/en/Tutorial/Foundations/Memory)
