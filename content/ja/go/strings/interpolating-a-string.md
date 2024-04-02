---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:58:40.232397-07:00
description: "\u6587\u5B57\u5217\u88DC\u9593\u306F\u3001\u5909\u6570\u3092\u53D6\u308A\
  \u8FBC\u3093\u3067\u52D5\u7684\u306B\u6587\u5B57\u5217\u3092\u69CB\u7BC9\u3059\u308B\
  \u65B9\u6CD5\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30E1\
  \u30C3\u30BB\u30FC\u30B8\u306E\u30AB\u30B9\u30BF\u30DE\u30A4\u30BA\u3001URL\u306E\
  \u69CB\u7BC9\u3001SQL\u30AF\u30A8\u30EA\u306E\u4F5C\u6210\u306A\u3069\u3092\u884C\
  \u3046\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002\u3053\u308C\
  \u306B\u3088\u308A\u3001\u30B3\u30FC\u30C9\u304C\u8AAD\u307F\u3084\u3059\u304F\u3001\
  \u4FDD\u5B88\u3057\u3084\u3059\u304F\u306A\u308A\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:41.363222-06:00'
model: gpt-4-0125-preview
summary: "\u6587\u5B57\u5217\u88DC\u9593\u306F\u3001\u5909\u6570\u3092\u53D6\u308A\
  \u8FBC\u3093\u3067\u52D5\u7684\u306B\u6587\u5B57\u5217\u3092\u69CB\u7BC9\u3059\u308B\
  \u65B9\u6CD5\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30E1\
  \u30C3\u30BB\u30FC\u30B8\u306E\u30AB\u30B9\u30BF\u30DE\u30A4\u30BA\u3001URL\u306E\
  \u69CB\u7BC9\u3001SQL\u30AF\u30A8\u30EA\u306E\u4F5C\u6210\u306A\u3069\u3092\u884C\
  \u3046\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002\u3053\u308C\
  \u306B\u3088\u308A\u3001\u30B3\u30FC\u30C9\u304C\u8AAD\u307F\u3084\u3059\u304F\u3001\
  \u4FDD\u5B88\u3057\u3084\u3059\u304F\u306A\u308A\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u306E\u88DC\u9593"
weight: 8
---

## 何となぜ？

文字列補間は、変数を取り込んで動的に文字列を構築する方法です。プログラマーは、メッセージのカスタマイズ、URLの構築、SQLクエリの作成などを行うためにこれを行います。これにより、コードが読みやすく、保守しやすくなります。

## どのようにして：

Goでは、文字列補間は一般的に`fmt`パッケージを使用して行われ、特に`Sprintf`関数を使用して変数を文字列に挿入します。これは、フォーマット文字列にフォーマット指示子を指定することで実現されます。指示子はフォーマット文字列のプレースホルダーであり、与えられた変数の値によって置き換えられます。以下のように使用します：

```go
package main

import (
    "fmt"
)

func main() {
    name := "Jane"
    age := 28

    // Sprintfを使用した文字列補間
    message := fmt.Sprintf("Hello, my name is %s and I am %d years old.", name, age)
    fmt.Println(message) // 出力: Hello, my name is Jane and I am 28 years old.
}
```

`%s`は文字列用、`%d`は整数用です。`fmt`パッケージのドキュメントには、異なるデータ型に対するフォーマット指示子の包括的なリストが提供されています。

## より深く

文字列補間の概念は、様々なプログラミング言語に存在しますが、それぞれ異なる構文と機能を持っています。Goでは、`fmt`パッケージの`Sprintf`関数が最も一般的に使用される方法ですが、特にシンプルな連結や、パフォーマンスに非常に敏感なコード内で作業する場合には、常に最も効率的な方法とは限りません。

`fmt`パッケージは、実行時に変数の型を動的に解釈するためにリフレクションを使用しますが、柔軟性がある一方で、オーバーヘッドが発生します。パフォーマンスが重要なシナリオでは、直接の文字列連結や`strings.Builder`型がより良い代替手段を提供する場合があります。直接の連結は単純ですが、複数の変数がある場合には取り扱いが面倒になることがあります。一方、`strings.Builder`は、多くの変数を扱う場合やループ内で複雑な文字列を構築する際に、よりパフォーマンスが高く、読みやすい方法を提供します：

```go
var sb strings.Builder
sb.WriteString("Hello, my name is ")
sb.WriteString(name)
sb.WriteString(" and I am ")
sb.WriteString(strconv.Itoa(age))
sb.WriteString(" years old.")
message := sb.String()

fmt.Println(message) // 以前と同様の出力
```

最終的に、`fmt.Sprintf`、直接の連結、そして`strings.Builder`の選択は、アプリケーションの具体的な要件、例えば構築される文字列の複雑さやパフォーマンスの考慮事項に依存します。
