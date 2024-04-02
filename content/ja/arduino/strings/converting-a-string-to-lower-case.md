---
date: 2024-01-20 17:37:38.990355-07:00
description: "\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB\u3059\u308B\
  \u3068\u306F\u3001\u5927\u6587\u5B57\u3092\u5C0F\u6587\u5B57\u306B\u4E00\u62EC\u3057\
  \u3066\u5909\u66F4\u3059\u308B\u3053\u3068\u3067\u3059\u3002\u3053\u308C\u3092\u30D7\
  \u30ED\u30B0\u30E9\u30DE\u30FC\u304C\u884C\u3046\u7406\u7531\u306F\u3001\u5165\u529B\
  \u306E\u4E00\u8CAB\u6027\u3092\u4FDD\u3061\u3001\u5927\u6587\u5B57\u3068\u5C0F\u6587\
  \u5B57\u306E\u9055\u3044\u306B\u3088\u308B\u30A8\u30E9\u30FC\u3092\u9632\u3050\u305F\
  \u3081\u3067\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.476480-06:00'
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB\u3059\u308B\
  \u3068\u306F\u3001\u5927\u6587\u5B57\u3092\u5C0F\u6587\u5B57\u306B\u4E00\u62EC\u3057\
  \u3066\u5909\u66F4\u3059\u308B\u3053\u3068\u3067\u3059\u3002\u3053\u308C\u3092\u30D7\
  \u30ED\u30B0\u30E9\u30DE\u30FC\u304C\u884C\u3046\u7406\u7531\u306F\u3001\u5165\u529B\
  \u306E\u4E00\u8CAB\u6027\u3092\u4FDD\u3061\u3001\u5927\u6587\u5B57\u3068\u5C0F\u6587\
  \u5B57\u306E\u9055\u3044\u306B\u3088\u308B\u30A8\u30E9\u30FC\u3092\u9632\u3050\u305F\
  \u3081\u3067\u3059\u3002"
title: "\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB"
weight: 4
---

## What & Why? (何となぜ？)
文字列を小文字に変換するとは、大文字を小文字に一括して変更することです。これをプログラマーが行う理由は、入力の一貫性を保ち、大文字と小文字の違いによるエラーを防ぐためです。

## How to: (方法)
```Arduino
void setup() {
  Serial.begin(9600);  // シリアル通信の初期化
  String myString = "HeLLo, ArDuinO!";  // 変換する文字列
  myString.toLowerCase();  // 文字列を小文字に変換
  Serial.println(myString);  // 結果を表示
}

void loop() {
  // ループは使いません
}
```
サンプル出力:
```
hello, arduino!
```

## Deep Dive (深堀り)
歴史的に、大文字と小文字の区別は人間にとって意味がありましたが、コンピューターではしばしば問題を引き起こします。例えば、ユーザー名やメールアドレスなどでの大文字小文字の混在です。代替方法はありますが、`String` オブジェクトの `toLowerCase()` メソッドがArduinoで文字列を変換する最も直接的な方法です。このメソッドは文字列の各文字にASCIIルールを適用し、大文字をそれに対応する小文字に変換します。

## See Also (関連情報)
- Arduino String reference: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- ASCII table and description: https://www.asciitable.com/
- More about string manipulation: https://www.arduino.cc/en/Tutorial/BuiltInExamples/StringAdditionOperator

Arduinoの世界では、コードがシンプルで機能的であることが重要です。これらのリソースを使って、文字列の扱い方を更に学んでみてください。
