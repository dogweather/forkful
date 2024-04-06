---
date: 2024-01-20 17:37:38.990355-07:00
description: "How to: (\u65B9\u6CD5) \u6B74\u53F2\u7684\u306B\u3001\u5927\u6587\u5B57\
  \u3068\u5C0F\u6587\u5B57\u306E\u533A\u5225\u306F\u4EBA\u9593\u306B\u3068\u3063\u3066\
  \u610F\u5473\u304C\u3042\u308A\u307E\u3057\u305F\u304C\u3001\u30B3\u30F3\u30D4\u30E5\
  \u30FC\u30BF\u30FC\u3067\u306F\u3057\u3070\u3057\u3070\u554F\u984C\u3092\u5F15\u304D\
  \u8D77\u3053\u3057\u307E\u3059\u3002\u4F8B\u3048\u3070\u3001\u30E6\u30FC\u30B6\u30FC\
  \u540D\u3084\u30E1\u30FC\u30EB\u30A2\u30C9\u30EC\u30B9\u306A\u3069\u3067\u306E\u5927\
  \u6587\u5B57\u5C0F\u6587\u5B57\u306E\u6DF7\u5728\u3067\u3059\u3002\u4EE3\u66FF\u65B9\
  \u6CD5\u306F\u3042\u308A\u307E\u3059\u304C\u3001`String` \u30AA\u30D6\u30B8\u30A7\
  \u30AF\u30C8\u306E `toLowerCase()`\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:50:56.368183-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) \u6B74\u53F2\u7684\u306B\u3001\u5927\u6587\u5B57\u3068\u5C0F\
  \u6587\u5B57\u306E\u533A\u5225\u306F\u4EBA\u9593\u306B\u3068\u3063\u3066\u610F\u5473\
  \u304C\u3042\u308A\u307E\u3057\u305F\u304C\u3001\u30B3\u30F3\u30D4\u30E5\u30FC\u30BF\
  \u30FC\u3067\u306F\u3057\u3070\u3057\u3070\u554F\u984C\u3092\u5F15\u304D\u8D77\u3053\
  \u3057\u307E\u3059\u3002\u4F8B\u3048\u3070\u3001\u30E6\u30FC\u30B6\u30FC\u540D\u3084\
  \u30E1\u30FC\u30EB\u30A2\u30C9\u30EC\u30B9\u306A\u3069\u3067\u306E\u5927\u6587\u5B57\
  \u5C0F\u6587\u5B57\u306E\u6DF7\u5728\u3067\u3059\u3002\u4EE3\u66FF\u65B9\u6CD5\u306F\
  \u3042\u308A\u307E\u3059\u304C\u3001`String` \u30AA\u30D6\u30B8\u30A7\u30AF\u30C8\
  \u306E `toLowerCase()` \u30E1\u30BD\u30C3\u30C9\u304CArduino\u3067\u6587\u5B57\u5217\
  \u3092\u5909\u63DB\u3059\u308B\u6700\u3082\u76F4\u63A5\u7684\u306A\u65B9\u6CD5\u3067\
  \u3059\u3002\u3053\u306E\u30E1\u30BD\u30C3\u30C9\u306F\u6587\u5B57\u5217\u306E\u5404\
  \u6587\u5B57\u306BASCII\u30EB\u30FC\u30EB\u3092\u9069\u7528\u3057\u3001\u5927\u6587\
  \u5B57\u3092\u305D\u308C\u306B\u5BFE\u5FDC\u3059\u308B\u5C0F\u6587\u5B57\u306B\u5909\
  \u63DB\u3057\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB"
weight: 4
---

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
