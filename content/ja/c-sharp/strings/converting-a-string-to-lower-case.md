---
date: 2024-01-20 17:37:53.944411-07:00
description: "\u4F55\u3068\u306A\u305C\uFF1F \u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\
  \u306B\u5909\u63DB\u3059\u308B\u3053\u3068\u306F\u3001\u30D7\u30ED\u30B0\u30E9\u30DF\
  \u30F3\u30B0\u306B\u304A\u3044\u3066\u3001\u5927\u6587\u5B57\u3068\u5C0F\u6587\u5B57\
  \u3092\u533A\u5225\u3057\u306A\u3044\u6BD4\u8F03\u3092\u3057\u305F\u3044\u6642\u3084\
  \u3001\u30C7\u30FC\u30BF\u306E\u4E00\u8CAB\u6027\u3092\u4FDD\u3064\u305F\u3081\u306B\
  \u884C\u3044\u307E\u3059\u3002\u30E6\u30FC\u30B6\u30FC\u304C\u5165\u529B\u3057\u305F\
  \u30C6\u30AD\u30B9\u30C8\u3092\u6574\u5F62\u3059\u308B\u969B\u306B\u3082\u4F7F\u308F\
  \u308C\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T22:38:41.649188-06:00'
model: gpt-4-1106-preview
summary: "\u4F55\u3068\u306A\u305C\uFF1F \u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\
  \u306B\u5909\u63DB\u3059\u308B\u3053\u3068\u306F\u3001\u30D7\u30ED\u30B0\u30E9\u30DF\
  \u30F3\u30B0\u306B\u304A\u3044\u3066\u3001\u5927\u6587\u5B57\u3068\u5C0F\u6587\u5B57\
  \u3092\u533A\u5225\u3057\u306A\u3044\u6BD4\u8F03\u3092\u3057\u305F\u3044\u6642\u3084\
  \u3001\u30C7\u30FC\u30BF\u306E\u4E00\u8CAB\u6027\u3092\u4FDD\u3064\u305F\u3081\u306B\
  \u884C\u3044\u307E\u3059\u3002\u30E6\u30FC\u30B6\u30FC\u304C\u5165\u529B\u3057\u305F\
  \u30C6\u30AD\u30B9\u30C8\u3092\u6574\u5F62\u3059\u308B\u969B\u306B\u3082\u4F7F\u308F\
  \u308C\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB"
weight: 4
---

## 何となぜ？
文字列を小文字に変換することは、プログラミングにおいて、大文字と小文字を区別しない比較をしたい時や、データの一貫性を保つために行います。ユーザーが入力したテキストを整形する際にも使われます。

## How to:


## 方法:
```C#
string originalText = "Konnichiwa, Sekai!";
string lowerCaseText = originalText.ToLower();

Console.WriteLine(lowerCaseText);
// 出力: konnichiwa, sekai!
```

## Deep Dive


## 詳細情報:
小文字変換は長い歴史を持ち、プログラミング言語が発展するにつれて進化してきました。C#では`ToLower()`や`ToLowerInvariant()`といったメソッドを使って実現します。`ToLowerInvariant()`はカルチャーに依存しないケース変換を提供し、異なるカルチャー間でも一貫性を保つために使用されます。中には`TextInfo`クラスを使ったより細かい制御を行う方法もありますが、ほとんどの場面では`ToLower()`で事足ります。

## See Also


## 関連リンク:
- [Microsoft's official ToLower documentation](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower?view=net-6.0)
- [Microsoft's guide to globalization and localization](https://docs.microsoft.com/en-us/dotnet/standard/globalization-localization/)
- [Stack Overflow: Why use ToLowerInvariant over ToLower?](https://stackoverflow.com/questions/6225808/string-tolower-and-string-tolowerinvariant)
