---
date: 2024-01-20 17:33:57.554254-07:00
description: ''
isCJKLanguage: true
lastmod: '2024-04-05T21:59:54.706918-06:00'
model: gpt-4-1106-preview
summary: "(\u3084\u308A\u65B9) \u6614\u3001Arduino\u3067\u306F\u30E1\u30E2\u30EA\u304C\
  \u5C11\u306A\u304B\u3063\u305F\u305F\u3081\u3001\u6587\u5B57\u5217\u306E\u9023\u7D50\
  \u306B\u306F\u6CE8\u610F\u304C\u5FC5\u8981\u3067\u3057\u305F\u3002\u4EE3\u66FF\u624B\
  \u6BB5\u3068\u3057\u3066`char`\u914D\u5217\u3068\u95A2\u6570`strcat()`\u304C\u4F7F\
  \u308F\u308C\u3066\u304D\u307E\u3057\u305F\u304C\u3001\u6271\u3044\u306B\u304F\u3044\
  \u3067\u3059\u3002\u73FE\u5728\u306EArduino\u3067\u306F`String`\u30AF\u30E9\u30B9\
  \u3092\u4F7F\u7528\u3059\u308B\u3053\u3068\u3067\u3001\u7C21\u5358\u306B\u6587\u5B57\
  \u5217\u306E\u9023\u7D50\u304C\u3067\u304D\u307E\u3059\u3002\u305F\u3060\u3057\u3001\
  \u80CC\u5F8C\u3067\u30E1\u30E2\u30EA\u306E\u52D5\u7684\u78BA\u4FDD\u304C\u884C\u308F\
  \u308C\u308B\u305F\u3081\u3001\u4F7F\u3044\u3059\u304E\u308B\u3068\u30E1\u30E2\u30EA\
  \u65AD\u7247\u5316\u306E\u554F\u984C\u304C\u8D77\u3053\u308B\u53EF\u80FD\u6027\u304C\
  \u307E\u3060\u3042\u308A\u307E\u3059\u3002"
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
