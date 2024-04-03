---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:37.637141-07:00
description: "\u6587\u5B57\u5217\u5185\u306E\u7279\u5B9A\u306E\u30D1\u30BF\u30FC\u30F3\
  \u306B\u4E00\u81F4\u3059\u308B\u6587\u5B57\u3092\u524A\u9664\u3059\u308B\u3053\u3068\
  \u306F\u3001\u30C7\u30FC\u30BF\u691C\u8A3C\u3001\u30B5\u30CB\u30BF\u30A4\u30BA\u3001\
  \u3042\u308B\u3044\u306F\u30C6\u30AD\u30B9\u30C8\u3092\u3055\u3089\u306A\u308B\u51E6\
  \u7406\u306E\u305F\u3081\u306B\u6E96\u5099\u3059\u308B\u969B\u306B\u4E0D\u53EF\u6B20\
  \u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u306F\u3053\u306E\u30BF\u30B9\u30AF\
  \u3092\u5B9F\u884C\u3059\u308B\u3053\u3068\u3067\u3001\u30C7\u30FC\u30BF\u306E\u6574\
  \u5408\u6027\u3092\u4FDD\u8A3C\u3057\u3001\u53EF\u8AAD\u6027\u3092\u5411\u4E0A\u3055\
  \u305B\u3001\u30C6\u30AD\u30B9\u30C8\u5165\u529B\u5168\u4F53\u306B\u308F\u305F\u3063\
  \u3066\u4E00\u8CAB\u3057\u305F\u5F62\u5F0F\u3092\u5F37\u5236\u3059\u308B\u3002"
lastmod: '2024-03-13T22:44:41.645673-06:00'
model: gpt-4-0125-preview
summary: "\u6587\u5B57\u5217\u5185\u306E\u7279\u5B9A\u306E\u30D1\u30BF\u30FC\u30F3\
  \u306B\u4E00\u81F4\u3059\u308B\u6587\u5B57\u3092\u524A\u9664\u3059\u308B\u3053\u3068\
  \u306F\u3001\u30C7\u30FC\u30BF\u691C\u8A3C\u3001\u30B5\u30CB\u30BF\u30A4\u30BA\u3001\
  \u3042\u308B\u3044\u306F\u30C6\u30AD\u30B9\u30C8\u3092\u3055\u3089\u306A\u308B\u51E6\
  \u7406\u306E\u305F\u3081\u306B\u6E96\u5099\u3059\u308B\u969B\u306B\u4E0D\u53EF\u6B20\
  \u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u306F\u3053\u306E\u30BF\u30B9\u30AF\
  \u3092\u5B9F\u884C\u3059\u308B\u3053\u3068\u3067\u3001\u30C7\u30FC\u30BF\u306E\u6574\
  \u5408\u6027\u3092\u4FDD\u8A3C\u3057\u3001\u53EF\u8AAD\u6027\u3092\u5411\u4E0A\u3055\
  \u305B\u3001\u30C6\u30AD\u30B9\u30C8\u5165\u529B\u5168\u4F53\u306B\u308F\u305F\u3063\
  \u3066\u4E00\u8CAB\u3057\u305F\u5F62\u5F0F\u3092\u5F37\u5236\u3059\u308B\u3002."
title: "\u30D1\u30BF\u30FC\u30F3\u306B\u4E00\u81F4\u3059\u308B\u6587\u5B57\u306E\u524A\
  \u9664"
weight: 5
---

## 方法:
Dartでは、正規表現と`replaceAll`メソッドを使用して、事前定義されたパターンに一致する文字を簡単に削除できます。基本的な使用法にサードパーティのライブラリは不要で、このアプローチは非常にアクセスしやすいものとなっています。

以下は、文字列から数字を削除する方法を示す簡単な例です。

```dart
void main() {
  String stringWithDigits = 'Dart123 は楽しい456';
  // すべての数字に一致する正規表現パターンを定義する
  RegExp digitPattern = RegExp(r'\d');
  
  // パターンに一致するすべての箇所を空文字列で置き換える
  String result = stringWithDigits.replaceAll(digitPattern, '');
  
  print(result); // 出力: Dart は楽しい
}
```

スペースと句読点を除く特殊文字を削除するような、より複雑なシナリオに取り組んでいる場合は、次の方法で実行します：

```dart
void main() {
  String messyString = 'Dart!@# is *&()fun$%^';
  // 文字、数字、スペース、および句読点を除くすべてに一致するパターンを定義する
  RegExp specialCharPattern = RegExp(r'[^a-zA-Z0-9 \.,!?]');
  
  String cleanedString = messyString.replaceAll(specialCharPattern, '');
  
  print(cleanedString); // 出力: Dart! is fun
}
```

より高度なパターンマッチングと置換が必要なタスクに対して、Dartの包括的な`RegExp`クラスの文書化は、より複雑な表現とその使用方法について深く掘り下げます。しかし、上記の例は、Dartプログラミングにおけるパターンに基づく文字の削除のための一般的なユースケースの大半を網羅しています。
