---
date: 2024-01-26 01:09:57.638052-07:00
description: "\u30B3\u30FC\u30C9\u3092\u95A2\u6570\u306B\u5206\u5272\u3059\u308B\u3053\
  \u3068\u306F\u3001LEGO\u30D6\u30ED\u30C3\u30AF\u3092\u5206\u985E\u3059\u308B\u3088\
  \u3046\u306A\u3082\u306E\u3067\u3059\u3002\u63A2\u3057\u3084\u3059\u304F\u306A\u308A\
  \u3001\u5229\u7528\u3082\u7C21\u5358\u306B\u306A\u308A\u307E\u3059\u3002\u3053\u308C\
  \u3092\u884C\u3046\u7406\u7531\u306F\u3001\u7E70\u308A\u8FD4\u3057\u3092\u907F\u3051\
  \u308B\u305F\u3081\u3001\u7406\u89E3\u3092\u5358\u7D14\u5316\u3059\u308B\u305F\u3081\
  \u3001\u305D\u3057\u3066\u30E1\u30F3\u30C6\u30CA\u30F3\u30B9\u3092\u982D\u75DB\u306E\
  \u7A2E\u306B\u3057\u306A\u3044\u305F\u3081\u3067\u3059\u3002"
lastmod: '2024-03-13T22:44:42.131451-06:00'
model: gpt-4-1106-preview
summary: "\u30B3\u30FC\u30C9\u3092\u95A2\u6570\u306B\u5206\u5272\u3059\u308B\u3053\
  \u3068\u306F\u3001LEGO\u30D6\u30ED\u30C3\u30AF\u3092\u5206\u985E\u3059\u308B\u3088\
  \u3046\u306A\u3082\u306E\u3067\u3059\u3002\u63A2\u3057\u3084\u3059\u304F\u306A\u308A\
  \u3001\u5229\u7528\u3082\u7C21\u5358\u306B\u306A\u308A\u307E\u3059\u3002\u3053\u308C\
  \u3092\u884C\u3046\u7406\u7531\u306F\u3001\u7E70\u308A\u8FD4\u3057\u3092\u907F\u3051\
  \u308B\u305F\u3081\u3001\u7406\u89E3\u3092\u5358\u7D14\u5316\u3059\u308B\u305F\u3081\
  \u3001\u305D\u3057\u3066\u30E1\u30F3\u30C6\u30CA\u30F3\u30B9\u3092\u982D\u75DB\u306E\
  \u7A2E\u306B\u3057\u306A\u3044\u305F\u3081\u3067\u3059\u3002"
title: "\u30B3\u30FC\u30C9\u3092\u95A2\u6570\u306B\u6574\u7406\u3059\u308B"
---

{{< edit_this_page >}}

## 何となく理由
コードを関数に分割することは、LEGOブロックを分類するようなものです。探しやすくなり、利用も簡単になります。これを行う理由は、繰り返しを避けるため、理解を単純化するため、そしてメンテナンスを頭痛の種にしないためです。

## 方法
複数回、あいさつを出力するコードがあったとしましょう。関数を使用しない場合は、ごちゃごちゃしていますが、関数を使用するときれいになります。

```C#
// 関数を使用せず - 繰り返しが多い
Console.WriteLine("こんにちは、エイミー！");
Console.WriteLine("こんにちは、ボブ！");
Console.WriteLine("こんにちは、チャーリー！");

// 関数を使用して - よりすっきり
void Greet(string name) {
    Console.WriteLine($"こんにちは、{name}！");
}

Greet("エイミー");
Greet("ボブ");
Greet("チャーリー");
```

出力はどちらも同じですが、2番目のバージョンの方がずっと整頓されています。

## 深掘り
昔、アセンブリ言語の時代には、GOTOを使用して異なるコードの位置に移動していましたが、これは混沌としており追跡が困難でした。関数は、ツールボックスの整理された引き出しのような、大きなレベルアップです。代替手段？もちろんあります。クラスの文脈において関数はメソッドとなりますし、ラムダやインライン関数はその場限りの短いタスクに利用できます。

実装については、小さく集中した関数が最高です。テストやデバッグが容易になります。多くの責任を持つ大きな関数は手に負えないものになる可能性があり、「スパゲッティコード」という不名誉な称号を得ることがあります。関数には一つの仕事をさせること；後で感謝するでしょう。

## 参考
関数とベストプラクティスについてもっと知りたい場合は、以下をチェックしてください：

- Robert C. Martinの「Clean Code」：関数をきれいに保つ原則。
- Martin Fowlerの「Refactoring」：既存のコードを改善する方法。
- Microsoft C#ガイド「Methods」: https://docs.microsoft.com/ja-jp/dotnet/csharp/programming-guide/classes-and-structs/methods
