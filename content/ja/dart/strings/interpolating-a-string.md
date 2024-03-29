---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:11.303405-07:00
description: "\u6587\u5B57\u5217\u88DC\u9593\u3068\u306F\u3001\u5909\u6570\u5024\u3092\
  \u76F4\u63A5\u6587\u5B57\u5217\u306B\u6CE8\u5165\u3059\u308B\u30D7\u30ED\u30BB\u30B9\
  \u3067\u3001\u9762\u5012\u306A\u9023\u7D50\u3092\u4F7F\u7528\u305B\u305A\u306B\u610F\
  \u5473\u306E\u3042\u308B\u30E1\u30C3\u30BB\u30FC\u30B8\u3092\u4F5C\u6210\u3059\u308B\
  \u3053\u3068\u304C\u3088\u304F\u3042\u308A\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u30FC\u306F\u3001\u3088\u308A\u30AF\u30EA\u30FC\u30F3\u3067\u8AAD\u307F\u3084\
  \u3059\u3044\u30B3\u30FC\u30C9\u3092\u5B9F\u73FE\u3057\u3001\u8907\u96D1\u306A\u6587\
  \u5B57\u5217\u306E\u9023\u7D50\u3067\u767A\u751F\u3057\u3084\u3059\u3044\u30A8\u30E9\
  \u30FC\u3092\u9632\u3050\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\
  \u3002"
lastmod: '2024-03-13T22:44:41.648063-06:00'
model: gpt-4-0125-preview
summary: "\u6587\u5B57\u5217\u88DC\u9593\u3068\u306F\u3001\u5909\u6570\u5024\u3092\
  \u76F4\u63A5\u6587\u5B57\u5217\u306B\u6CE8\u5165\u3059\u308B\u30D7\u30ED\u30BB\u30B9\
  \u3067\u3001\u9762\u5012\u306A\u9023\u7D50\u3092\u4F7F\u7528\u305B\u305A\u306B\u610F\
  \u5473\u306E\u3042\u308B\u30E1\u30C3\u30BB\u30FC\u30B8\u3092\u4F5C\u6210\u3059\u308B\
  \u3053\u3068\u304C\u3088\u304F\u3042\u308A\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u30FC\u306F\u3001\u3088\u308A\u30AF\u30EA\u30FC\u30F3\u3067\u8AAD\u307F\u3084\
  \u3059\u3044\u30B3\u30FC\u30C9\u3092\u5B9F\u73FE\u3057\u3001\u8907\u96D1\u306A\u6587\
  \u5B57\u5217\u306E\u9023\u7D50\u3067\u767A\u751F\u3057\u3084\u3059\u3044\u30A8\u30E9\
  \u30FC\u3092\u9632\u3050\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\
  \u3002"
title: "\u6587\u5B57\u5217\u306E\u88DC\u9593"
---

{{< edit_this_page >}}

## 何となぜ？

文字列補間とは、変数値を直接文字列に注入するプロセスで、面倒な連結を使用せずに意味のあるメッセージを作成することがよくあります。プログラマーは、よりクリーンで読みやすいコードを実現し、複雑な文字列の連結で発生しやすいエラーを防ぐためにこれを行います。

## 方法：

Dartでは、文字列補間は直感的で、`$` 記号を使用して文字列リテラル内に直接式を補間します：

```dart
void main() {
  String name = 'Dart';
  int year = 2023;
  // 単純な変数の補間
  print('Learning $name in $year!');
  // 出力: Learning Dart in 2023!
  
  // 式の補間
  print('In two years, it will be ${year + 2}.');
  // 出力: In two years, it will be 2025.
}
```

より複雑な式を持っている場合や、文字列自体で操作を行いたい場合は、式を `${}` で囲みます。Dartには文字列補間のための特に人気のあるサードパーティのライブラリはありませんが、さまざまで複雑なシナリオをネイティブにうまく扱えるように十分装備されています。
