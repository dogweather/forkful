---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:11.303405-07:00
description: "\u65B9\u6CD5\uFF1A Dart\u3067\u306F\u3001\u6587\u5B57\u5217\u88DC\u9593\
  \u306F\u76F4\u611F\u7684\u3067\u3001`$` \u8A18\u53F7\u3092\u4F7F\u7528\u3057\u3066\
  \u6587\u5B57\u5217\u30EA\u30C6\u30E9\u30EB\u5185\u306B\u76F4\u63A5\u5F0F\u3092\u88DC\
  \u9593\u3057\u307E\u3059\uFF1A."
lastmod: '2024-03-13T22:44:41.648063-06:00'
model: gpt-4-0125-preview
summary: "Dart\u3067\u306F\u3001\u6587\u5B57\u5217\u88DC\u9593\u306F\u76F4\u611F\u7684\
  \u3067\u3001`$` \u8A18\u53F7\u3092\u4F7F\u7528\u3057\u3066\u6587\u5B57\u5217\u30EA\
  \u30C6\u30E9\u30EB\u5185\u306B\u76F4\u63A5\u5F0F\u3092\u88DC\u9593\u3057\u307E\u3059\
  \uFF1A."
title: "\u6587\u5B57\u5217\u306E\u88DC\u9593"
weight: 8
---

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
