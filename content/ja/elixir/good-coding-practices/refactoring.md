---
date: 2024-01-26 01:17:54.687301-07:00
description: "\u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\u30B0\u3068\u306F\u3001\u5916\
  \u90E8\u306E\u632F\u308B\u821E\u3044\u3092\u5909\u3048\u305A\u306B\u65E2\u5B58\u306E\
  \u30B3\u30FC\u30C9\u3092\u518D\u69CB\u7BC9\u3059\u308B\u30D7\u30ED\u30BB\u30B9\u3067\
  \u3042\u308A\u3001\u8AAD\u307F\u3084\u3059\u3055\u3084\u4FDD\u5B88\u6027\u306E\u3088\
  \u3046\u306A\u975E\u6A5F\u80FD\u5C5E\u6027\u3092\u6539\u5584\u3059\u308B\u3053\u3068\
  \u3092\u76EE\u7684\u3068\u3057\u3066\u3044\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u306F\u3001\u30B3\u30FC\u30C9\u3092\u3088\u308A\u30AF\u30EA\u30A2\u306B\u3001\
  \u7406\u89E3\u3057\u3084\u3059\u304F\u3001\u52B9\u7387\u7684\u306B\u3059\u308B\u305F\
  \u3081\u3001\u305D\u3057\u3066\u5C06\u6765\u306E\u30A2\u30C3\u30D7\u30C7\u30FC\u30C8\
  \u3092\u5BB9\u6613\u306B\u3057\u3001\u30D0\u30B0\u306E\u30EA\u30B9\u30AF\u3092\u6E1B\
  \u3089\u3059\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
lastmod: '2024-03-11T00:14:15.257939-06:00'
model: gpt-4-0125-preview
summary: "\u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\u30B0\u3068\u306F\u3001\u5916\
  \u90E8\u306E\u632F\u308B\u821E\u3044\u3092\u5909\u3048\u305A\u306B\u65E2\u5B58\u306E\
  \u30B3\u30FC\u30C9\u3092\u518D\u69CB\u7BC9\u3059\u308B\u30D7\u30ED\u30BB\u30B9\u3067\
  \u3042\u308A\u3001\u8AAD\u307F\u3084\u3059\u3055\u3084\u4FDD\u5B88\u6027\u306E\u3088\
  \u3046\u306A\u975E\u6A5F\u80FD\u5C5E\u6027\u3092\u6539\u5584\u3059\u308B\u3053\u3068\
  \u3092\u76EE\u7684\u3068\u3057\u3066\u3044\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u306F\u3001\u30B3\u30FC\u30C9\u3092\u3088\u308A\u30AF\u30EA\u30A2\u306B\u3001\
  \u7406\u89E3\u3057\u3084\u3059\u304F\u3001\u52B9\u7387\u7684\u306B\u3059\u308B\u305F\
  \u3081\u3001\u305D\u3057\u3066\u5C06\u6765\u306E\u30A2\u30C3\u30D7\u30C7\u30FC\u30C8\
  \u3092\u5BB9\u6613\u306B\u3057\u3001\u30D0\u30B0\u306E\u30EA\u30B9\u30AF\u3092\u6E1B\
  \u3089\u3059\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
title: "\u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\u30B0"
---

{{< edit_this_page >}}

## 何となぜ？
リファクタリングとは、外部の振る舞いを変えずに既存のコードを再構築するプロセスであり、読みやすさや保守性のような非機能属性を改善することを目的としています。プログラマは、コードをよりクリアに、理解しやすく、効率的にするため、そして将来のアップデートを容易にし、バグのリスクを減らすためにこれを行います。

## 方法：
一般的なElixirパターンをきれいにしましょう。多くの役割を持ってしまっている`calculate_stats`関数を、より小さく、再利用可能な部分に分割してリファクタリングします。

```elixir
defmodule Stats do
  # オリジナルのリファクタリング前のコード
  def calculate_stats(data) do
    total = Enum.sum(data)
    count = Enum.count(data)
    mean = total / count
    {mean, total}
  end
  
  # リファクタリング後のコード
  def calculate_mean(data), do: Enum.sum(data) / Enum.count(data)
  
  def calculate_total(data), do: Enum.sum(data)
  
  def calculate_stats_refactored(data) do
    mean = calculate_mean(data)
    total = calculate_total(data)
    {mean, total}
  end
end

# サンプル出力
# リファクタリング前
Stats.calculate_stats([1, 2, 3])
# => {2.0, 6}

# リファクタリング後
Stats.calculate_stats_refactored([1, 2, 3])
# => {2.0, 6}
```
出力は同じままですが、今では独立してテストしやすいモジュラーな関数ができています。

## 深掘り
リファクタリングは新しい概念ではありません。ソフトウェア開発初期の頃からプログラミングの重要な部分でした。マーティン・ファウラーの「Refactoring: Improving the Design of Existing Code」などの著名な作品は、リファクタリングに対する基礎的な実践とそれらをいつ、どのように適用するかについての洞察を提供しています。

手動リファクタリングの代替手段には、リファクタリングを提案あるいは実行することができる自動コード分析ツールが含まれます。しかし、自動ツールは常にコードの完全なコンテキストを把握するわけではなく、人間のレビュアーが捉えることができる微妙な違いを見逃すことがあります。

Elixirにおける実装の詳細には、関数型パラダイムを理解し、パターンマッチング、ガード句、パイプ演算子を活用して、明瞭かつ簡潔なコードを記述することが含まれます。例えば、リファクタリングは、しばしば複雑な命令形式の関数を、Elixirが好むイミュータビリティと副作用のない操作に従うより小さく、組み合わせ可能な関数に変換することを含みます。

## 参照
Elixir固有のリファクタリング技術についての詳細は：

- [Elixirの公式ガイド](https://elixir-lang.org/getting-started/)
- [マーティン・ファウラーの「Refactoring: Improving the Design of Existing Code」](https://martinfowler.com/books/refactoring.html)、Elixirに適用できる一般原則について。
- [Credo, Elixirのための静的コード分析ツール](https://github.com/rrrene/credo)、ベストプラクティスを推奨します。
- [Exercism Elixir Track](https://exercism.org/tracks/elixir)、リファクタリングを含む実践的な演習のために。
