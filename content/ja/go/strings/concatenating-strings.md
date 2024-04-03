---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:54:13.220681-07:00
description: "\u6587\u5B57\u5217\u306E\u9023\u7D50\u3068\u306F\u30012\u3064\u4EE5\u4E0A\
  \u306E\u6587\u5B57\u5217\u3092\u7AEF\u304B\u3089\u7AEF\u307E\u3067\u7D50\u5408\u3057\
  \u3066\u65B0\u3057\u3044\u6587\u5B57\u5217\u3092\u5F62\u6210\u3059\u308B\u3053\u3068\
  \u3092\u6307\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\
  \u30E1\u30C3\u30BB\u30FC\u30B8\u3001\u30D1\u30B9\u3001\u8907\u96D1\u306A\u30AF\u30A8\
  \u30EA\u3092\u52D5\u7684\u306B\u751F\u6210\u3059\u308B\u305F\u3081\u306B\u3053\u306E\
  \u64CD\u4F5C\u3092\u884C\u3044\u3001\u30D7\u30ED\u30B0\u30E9\u30E0\u3092\u3088\u308A\
  \u30A4\u30F3\u30BF\u30E9\u30AF\u30C6\u30A3\u30D6\u3067\u53CD\u5FDC\u7684\u306B\u3057\
  \u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:41.373185-06:00'
model: gpt-4-0125-preview
summary: "\u6587\u5B57\u5217\u306E\u9023\u7D50\u3068\u306F\u30012\u3064\u4EE5\u4E0A\
  \u306E\u6587\u5B57\u5217\u3092\u7AEF\u304B\u3089\u7AEF\u307E\u3067\u7D50\u5408\u3057\
  \u3066\u65B0\u3057\u3044\u6587\u5B57\u5217\u3092\u5F62\u6210\u3059\u308B\u3053\u3068\
  \u3092\u6307\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\
  \u30E1\u30C3\u30BB\u30FC\u30B8\u3001\u30D1\u30B9\u3001\u8907\u96D1\u306A\u30AF\u30A8\
  \u30EA\u3092\u52D5\u7684\u306B\u751F\u6210\u3059\u308B\u305F\u3081\u306B\u3053\u306E\
  \u64CD\u4F5C\u3092\u884C\u3044\u3001\u30D7\u30ED\u30B0\u30E9\u30E0\u3092\u3088\u308A\
  \u30A4\u30F3\u30BF\u30E9\u30AF\u30C6\u30A3\u30D6\u3067\u53CD\u5FDC\u7684\u306B\u3057\
  \u307E\u3059\u3002."
title: "\u6587\u5B57\u5217\u306E\u9023\u7D50"
weight: 3
---

## 何となぜ？

文字列の連結とは、2つ以上の文字列を端から端まで結合して新しい文字列を形成することを指します。プログラマーは、メッセージ、パス、複雑なクエリを動的に生成するためにこの操作を行い、プログラムをよりインタラクティブで反応的にします。

## 方法:

Goには文字列を連結するいくつかの方法があります。以下にいくつかの一般的な方法と例を示します。

### `+` 演算子を使用:
文字列を連結する最もシンプルな方法は、`+` 演算子を使用することです。これは直接的ですが、複数の文字列に対しては最も効率的ではありません。
```go
firstName := "John"
lastName := "Doe"
fullName := firstName + " " + lastName
fmt.Println(fullName) // John Doe
```

### `fmt.Sprintf` を使用:
変数を含む文字列を整形するためには、`fmt.Sprintf` が非常に便利です。出力形式をより制御できます。
```go
age := 30
message := fmt.Sprintf("%sは%d歳です。", fullName, age)
fmt.Println(message) // John Doeは30歳です。
```

### `strings.Builder`を使用:
ループ内で複数の文字列を連結する場合、特に、`strings.Builder`は効率的で推奨されます。
```go
var builder strings.Builder
words := []string{"hello", "world", "from", "go"}

for _, word := range words {
    builder.WriteString(word)
    builder.WriteString(" ")
}

result := builder.String()
fmt.Println(result) // hello world from go
```

### `strings.Join`を使用:
特定のセパレータで結合されるべき文字列のスライスがある場合、`strings.Join`が最適な選択です。
```go
elements := []string{"path", "to", "file"}
path := strings.Join(elements, "/")
fmt.Println(path) // path/to/file
```

## 詳細な説明

文字列の連結は一見単純な操作のように見えますが、Goが文字列をどのように扱うかについてのより深い側面に触れます。Goでは、文字列は不変です。つまり、連結操作ごとに新しい文字列が作成されます。これは、大量の文字列を連結する場合や、タイトなループ内で連結を行う場合に、メモリの頻繁な割り当てとコピーによりパフォーマンスの問題を引き起こす可能性があります。

歴史的に、言語は文字列の不変性と連結の効率をさまざまな方法で対処してきましたが、Goの`strings.Builder`や`strings.Join`を使ったアプローチは、使いやすさとパフォーマンスのバランスをプログラマーに提供しています。特にGo 1.10で導入された`strings.Builder`型は、複数の文字列割り当てのオーバーヘッドを発生させることなく、文字列を構築する効率的な方法を提供する点で注目されます。これは、必要に応じて成長するバッファを割り当て、そのバッファに文字列を追加することによってこれを実現します。

これらのオプションにもかかわらず、文脈に基づいて正しい方法を選ぶことが重要です。簡単なまたは稀な連結の場合、単純な演算子や`fmt.Sprintf`で十分かもしれません。しかし、パフォーマンスが重要な場面、特に多くの連結が関わる場合には、`strings.Builder`や`strings.Join`を活用する方が適切かもしれません。

Goは文字列操作のための堅牢な組み込み機能を提供していますが、その背景にあるパフォーマンスの特性を意識することが重要です。`+`や`fmt.Sprintf`を通じた連結は、単純さや小規模な操作には適していますが、Goのより効率的な文字列構築の実践を理解し、活用することで、アプリケーションがパフォーマントでスケーラブルなままでいることを保証します。
