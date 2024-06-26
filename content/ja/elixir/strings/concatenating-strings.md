---
date: 2024-01-27 10:43:13.642297-07:00
description: "\u65B9\u6CD5\uFF1A Elixir\u3067\u306F\u3001\u3044\u304F\u3064\u304B\u306E\
  \u7C21\u5358\u306A\u65B9\u6CD5\u3067\u6587\u5B57\u5217\u3092\u9023\u7D50\u3067\u304D\
  \u307E\u3059\u3002\u6700\u3082\u4E00\u822C\u7684\u306A\u65B9\u6CD5\u3092\u898B\u3066\
  \u307F\u307E\u3057\u3087\u3046\uFF1A 1. `<>` \u30AA\u30DA\u30EC\u30FC\u30BF\u30FC\
  \u3092\u4F7F\u7528\u3059\u308B\u65B9\u6CD5\u306F\u3001\u6587\u5B57\u5217\u3092\u9023\
  \u7D50\u3059\u308B\u6700\u3082\u5358\u7D14\u3067\u76F4\u63A5\u7684\u306A\u65B9\u6CD5\
  \u3067\u3059\uFF1A."
lastmod: '2024-04-05T22:37:49.933708-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A Elixir\u3067\u306F\u3001\u3044\u304F\u3064\u304B\u306E\
  \u7C21\u5358\u306A\u65B9\u6CD5\u3067\u6587\u5B57\u5217\u3092\u9023\u7D50\u3067\u304D\
  \u307E\u3059\u3002\u6700\u3082\u4E00\u822C\u7684\u306A\u65B9\u6CD5\u3092\u898B\u3066\
  \u307F\u307E\u3057\u3087\u3046\uFF1A 1. `<>` \u30AA\u30DA\u30EC\u30FC\u30BF\u30FC\
  \u3092\u4F7F\u7528\u3059\u308B\u65B9\u6CD5\u306F\u3001\u6587\u5B57\u5217\u3092\u9023\
  \u7D50\u3059\u308B\u6700\u3082\u5358\u7D14\u3067\u76F4\u63A5\u7684\u306A\u65B9\u6CD5\
  \u3067\u3059\uFF1A."
title: "\u6587\u5B57\u5217\u306E\u9023\u7D50"
weight: 3
---

## 方法：
Elixirでは、いくつかの簡単な方法で文字列を連結できます。最も一般的な方法を見てみましょう：

1. `<>` オペレーターを使用する方法は、文字列を連結する最も単純で直接的な方法です：

```elixir
name = "Jane"
greeting = "Hello, " <> name <> "!"
IO.puts greeting
# 出力: Hello, Jane!
```

2. 文字列に変数を挿入したい場合、特に便利で、構文がよりクリアになるインターポレーションの使用：

```elixir
name = "John"
age = 28
introduction = "My name is #{name} and I am #{age} years old."
IO.puts introduction
# 出力: My name is John and I am 28 years old.
```

3. `Enum.join/2` 関数を使用した文字列リストの連結：

```elixir
parts = ["Elixir", " is", " awesome!"]
message = Enum.join(parts)
IO.puts message
# 出力: Elixir is awesome!
```

各方法はその文脈で光るものがありますので、ニーズに応じて選んでください。

## 深掘り
Elixirにおける文字列の連結は、多くの関数型言語と同様に、その特性により微妙な点があります。Elixirの不変性のため、文字列を連結するたびに実際には新しい文字列を作成しています。これは、文字列や特殊なバッファを可変とするCやJavaなどの言語が、繰り返し操作を効率的に処理できる場合に、パフォーマンスの問題を引き起こす可能性があります。

歴史的に、関数型言語で文字列の連結を効率的に扱う様々な戦略が考案されてきました。例えば、文字列をリストで蓄積して、連結操作を最後の瞬間まで行わないというのは一般的なパターンです。このアプローチは、Erlang（Elixirの基盤となるランタイムシステム）でリストが実装されている方法を利用して、より効率的なメモリ使用を可能にします。

Elixirは、中間の文字列を繰り返し連結することによるものを得ることなく、大量のテキストを効率的に生成できる代替手段として`IOList`を提供しています。IOListとは、文字列や文字コードのネストされたリストであり、それを最初に一緒に接着することなく、ファイルやネットワークなどの出力に直接書き込むことができるErlangの仮想マシン（BEAM）があります。

```elixir
content = ["Header", "\n", "Body text", "\n", "Footer"]
:ok = File.write("example.txt", content)
```

このスニペットでは、`content`はIOListであり、それを直接ファイルに書き込んでいます。このような操作は、最初にメモリ内でファイルの全内容を構築するために繰り返し文字列を連結する場合よりも、読みやすさと効率性の両方で劣ります。

これらの基本的な概念とツールを理解することは、Elixirでの文字列操作を扱う際の効率とパフォーマンスを大幅に向上させることができます。

## 参照
Elixirにおける文字列とパフォーマンスの詳細な読み物には、以下のリソースが役立ちます：

- [Elixirの公式ガイドのBinaries, Strings, and Charlists](https://elixir-lang.org/getting-started/binaries-strings-and-char-lists.html)
- [Erlang効率ガイド](http://erlang.org/doc/efficiency_guide/listHandling.html) - Erlangに特化していますが、Erlang VMの基盤上にあるElixirにも多くが当てはまります。
