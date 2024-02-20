---
date: 2024-01-20 17:50:41.261255-07:00
description: "\u6587\u5B57\u5217\u88DC\u9593\u3068\u306F\u3001\u6587\u5B57\u5217\u306E\
  \u4E2D\u306B\u76F4\u63A5\u5909\u6570\u306A\u3069\u3092\u57CB\u3081\u8FBC\u3080\u3053\
  \u3068\u3067\u3059\u3002\u4F7F\u3046\u7406\u7531\u306F\u7C21\u5358\u3067\u3001\u30B3\
  \u30FC\u30C9\u306E\u8AAD\u307F\u3084\u3059\u3055\u3092\u4FDD\u3061\u3064\u3064\u3001\
  \u52D5\u7684\u306B\u6587\u5B57\u5217\u306E\u5185\u5BB9\u3092\u69CB\u7BC9\u3067\u304D\
  \u308B\u304B\u3089\u3067\u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-19 22:05:01.255323
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u88DC\u9593\u3068\u306F\u3001\u6587\u5B57\u5217\u306E\
  \u4E2D\u306B\u76F4\u63A5\u5909\u6570\u306A\u3069\u3092\u57CB\u3081\u8FBC\u3080\u3053\
  \u3068\u3067\u3059\u3002\u4F7F\u3046\u7406\u7531\u306F\u7C21\u5358\u3067\u3001\u30B3\
  \u30FC\u30C9\u306E\u8AAD\u307F\u3084\u3059\u3055\u3092\u4FDD\u3061\u3064\u3064\u3001\
  \u52D5\u7684\u306B\u6587\u5B57\u5217\u306E\u5185\u5BB9\u3092\u69CB\u7BC9\u3067\u304D\
  \u308B\u304B\u3089\u3067\u3059\u3002"
title: "\u6587\u5B57\u5217\u306E\u88DC\u9593"
---

{{< edit_this_page >}}

## What & Why? (なぜ？とは？)
文字列補間とは、文字列の中に直接変数などを埋め込むことです。使う理由は簡単で、コードの読みやすさを保ちつつ、動的に文字列の内容を構築できるからです。

## How to: (方法)
```C#
string name = "Taro";
int age = 28;
string message = $"こんにちは、{name}さん。あなたは{age}歳ですね。";
Console.WriteLine(message);
```
出力:
```
こんにちは、Taroさん。あなたは28歳ですね。
```

## Deep Dive (探求)
昔は`String.Format()`や文字列の連結を利用していましたが、C# 6.0から文字列補間が導入され、コードが格段に読みやすくなりました。補間は`$`記号を使い、中括弧`{}`で変数を囲むだけ。内部的には`String.Format()`が使用されているので、パフォーマンス上の大きな違いはありません。ただし、コンパイラが生成するILコードは若干異なることがあるので面白いですね。他の方法としては、`StringBuilder`を使う方法もありますが、簡単に済ませるには文字列補間がベストです。

## See Also (関連情報)
- Microsoftの公式ドキュメント: [String interpolation (C# Reference)](https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/tokens/interpolated)
