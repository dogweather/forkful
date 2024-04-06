---
date: 2024-01-20 17:45:13.559736-07:00
description: ''
isCJKLanguage: true
lastmod: '2024-04-05T21:59:54.703582-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) Arduino\u8A00\u8A9E\u3067\u306F`String`\u30AF\u30E9\u30B9\
  \u306E`.substring()`\u30E1\u30BD\u30C3\u30C9\u3092\u4F7F\u3063\u3066\u6587\u5B57\
  \u5217\u304B\u3089\u90E8\u5206\u6587\u5B57\u5217\u3092\u629C\u304D\u53D6\u308A\u307E\
  \u3059\u3002\u3053\u306E\u6A5F\u80FD\u306FJava\u8A00\u8A9E\u7531\u6765\u3067\u3001\
  \u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u521D\u671F\u304B\u3089\u3042\u308B\u6982\
  \u5FF5\u3067\u3059\u3002\u4EE3\u66FF\u624B\u6BB5\u3068\u3057\u3066\u3001C\u30B9\u30BF\
  \u30A4\u30EB\u306E\u6587\u5B57\u914D\u5217\u3068\u95A2\u6570\uFF08`strncpy()`\u306A\
  \u3069\uFF09\u3092\u4F7F\u3046\u65B9\u6CD5\u304C\u3042\u308A\u307E\u3059\u304C\u3001\
  \u4F5C\u696D\u306F\u3088\u308A\u8907\u96D1\u3067\u3059\u3002`String`\u30AF\u30E9\
  \u30B9\u304C\u5C0E\u5165\u3055\u308C\u305F\u3053\u3068\u3067\u3001\u9AD8\u30EC\u30D9\
  \u30EB\u306E\u64CD\u4F5C\u304C\u7C21\u5358\u306B\u306A\u308A\u307E\u3057\u305F\u3002\
  \u3057\u304B\u3057\u3001`String`\u3092\u4F7F\u3046\u3068\u30E1\u30E2\u30EA\u30D5\
  \u30E9\u30B0\u30E1\u30F3\u30C6\u30FC\u30B7\u30E7\u30F3\u306E\u53EF\u80FD\u6027\u304C\
  \u5897\u3048\u308B\u305F\u3081\u3001\u30E1\u30E2\u30EA\u4F7F\u7528\u306B\u306F\u6CE8\
  \u610F\u304C\u5FC5\u8981\u3067\u3059\u3002"
title: "\u90E8\u5206\u6587\u5B57\u5217\u306E\u62BD\u51FA"
weight: 6
---

## How to: (方法)
```Arduino
String text = "Arduino programming is fun!";
String subText = text.substring(17, 20);

void setup() {
  Serial.begin(9600);
  Serial.println(subText); // Prints "fun"
}

void loop() {
  // Nothing here for now.
}
```

## Deep Dive (深掘り)
Arduino言語では`String`クラスの`.substring()`メソッドを使って文字列から部分文字列を抜き取ります。この機能はJava言語由来で、プログラミング初期からある概念です。代替手段として、Cスタイルの文字配列と関数（`strncpy()`など）を使う方法がありますが、作業はより複雑です。`String`クラスが導入されたことで、高レベルの操作が簡単になりました。しかし、`String`を使うとメモリフラグメンテーションの可能性が増えるため、メモリ使用には注意が必要です。

## See Also (関連情報)
- Arduino公式リファレンス：`String.substring()`に関する詳細 - [https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/substring/](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/substring/)
- 同様の操作を行うC言語関数に関する詳細 - [http://www.cplusplus.com/reference/cstring/strncpy/](http://www.cplusplus.com/reference/cstring/strncpy/)
- 文字列操作についての一般的なチュートリアル - [https://www.learncpp.com/cpp-tutorial/c-style-strings/](https://www.learncpp.com/cpp-tutorial/c-style-strings/)
