---
date: 2024-01-20 17:45:13.559736-07:00
description: "\u90E8\u5206\u6587\u5B57\u5217\u3092\u629C\u304D\u53D6\u308B\u3053\u3068\
  \u306F\u3001\u6587\u5B57\u5217\u304B\u3089\u7279\u5B9A\u306E\u7BC4\u56F2\u3092\u53D6\
  \u308A\u51FA\u3059\u30D7\u30ED\u30BB\u30B9\u3067\u3059\u3002\u3053\u308C\u3092\u884C\
  \u3046\u7406\u7531\u306F\u3001\u30C7\u30FC\u30BF\u89E3\u6790\u3001\u5165\u529B\u306E\
  \u30D0\u30EA\u30C7\u30FC\u30B7\u30E7\u30F3\u3001\u3042\u308B\u3044\u306F\u8868\u793A\
  \u5185\u5BB9\u306E\u30AB\u30B9\u30BF\u30DE\u30A4\u30BA\u306E\u305F\u3081\u3067\u3059\
  \u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.480272-06:00'
model: gpt-4-1106-preview
summary: "\u90E8\u5206\u6587\u5B57\u5217\u3092\u629C\u304D\u53D6\u308B\u3053\u3068\
  \u306F\u3001\u6587\u5B57\u5217\u304B\u3089\u7279\u5B9A\u306E\u7BC4\u56F2\u3092\u53D6\
  \u308A\u51FA\u3059\u30D7\u30ED\u30BB\u30B9\u3067\u3059\u3002\u3053\u308C\u3092\u884C\
  \u3046\u7406\u7531\u306F\u3001\u30C7\u30FC\u30BF\u89E3\u6790\u3001\u5165\u529B\u306E\
  \u30D0\u30EA\u30C7\u30FC\u30B7\u30E7\u30F3\u3001\u3042\u308B\u3044\u306F\u8868\u793A\
  \u5185\u5BB9\u306E\u30AB\u30B9\u30BF\u30DE\u30A4\u30BA\u306E\u305F\u3081\u3067\u3059\
  \u3002."
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
