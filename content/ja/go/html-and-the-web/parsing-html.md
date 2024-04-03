---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:05:46.466731-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:41.383491-06:00'
model: gpt-4-0125-preview
summary: "Go\u3067HTML\u3092\u30D1\u30FC\u30B9\u3059\u308B\u3068\u306F\u3001HTML\u30D5\
  \u30A1\u30A4\u30EB\u306E\u5185\u5BB9\u3092\u89E3\u6790\u3057\u3001\u30C7\u30FC\u30BF\
  \u3092\u62BD\u51FA\u3057\u305F\u308A\u3001\u69CB\u9020\u3092\u64CD\u4F5C\u3057\u305F\
  \u308A\u3001HTML\u3092\u4ED6\u306E\u5F62\u5F0F\u306B\u5909\u63DB\u3057\u305F\u308A\
  \u3059\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\
  \u3001\u30A6\u30A7\u30D6\u30B9\u30AF\u30EC\u30A4\u30D4\u30F3\u30B0\u3001\u30C6\u30F3\
  \u30D7\u30EC\u30FC\u30C8\u4F5C\u6210\u3001\u30C7\u30FC\u30BF\u30DE\u30A4\u30CB\u30F3\
  \u30B0\u306A\u3069\u306E\u76EE\u7684\u3067\u3053\u308C\u3092\u884C\u3044\u3001Go\u306E\
  \u5F37\u529B\u306A\u4E26\u884C\u51E6\u7406\u6A5F\u80FD\u3092\u6D3B\u7528\u3057\u3066\
  \u3001\u5927\u91CF\u306E\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u3092\u52B9\u7387\u7684\
  \u306B\u51E6\u7406\u3057\u307E\u3059\u3002."
title: "HTML\u306E\u89E3\u6790"
weight: 43
---

## 何となぜ？

GoでHTMLをパースするとは、HTMLファイルの内容を解析し、データを抽出したり、構造を操作したり、HTMLを他の形式に変換したりすることです。プログラマーは、ウェブスクレイピング、テンプレート作成、データマイニングなどの目的でこれを行い、Goの強力な並行処理機能を活用して、大量のウェブページを効率的に処理します。

## 方法：

GoでHTMLをパースするためには、通常、`goquery` パッケージまたは標準ライブラリの `net/html` パッケージを使用します。次は、`net/html` を使ってウェブページからすべてのリンクを抽出する基本的な例です：

```go
package main

import (
    "fmt"
    "golang.org/x/net/html"
    "net/http"
)

func main() {
    // HTMLドキュメントを取得
    res, err := http.Get("http://example.com")
    if err != nil {
        panic(err)
    }
    defer res.Body.Close()

    // HTMLドキュメントをパース
    doc, err := html.Parse(res.Body)
    if err != nil {
        panic(err)
    }

    // DOMを再帰的にトラバースする関数
    var f func(*html.Node)
    f = func(n *html.Node) {
        if n.Type == html.ElementNode && n.Data == "a" {
            for _, a := range n.Attr {
                if a.Key == "href" {
                    fmt.Println(a.Val)
                    break
                }
            }
        }
        for c := n.FirstChild; c != nil; c = c.NextSibling {
            f(c)
        }
    }

    // DOMをトラバース
    f(doc)
}
```

サンプル出力（`http://example.com` に二つのリンクが含まれていると仮定）：

```
http://www.iana.org/domains/example
http://www.iana.org/domains/reserved
```

このコードはHTMLページを要求し、パースし、DOMを再帰的にトラバースして、すべての `<a>` タグの `href` 属性を見つけて出力します。

## ディープダイブ

`net/html` パッケージは、HTML5標準で指定されたトークン化およびツリー構築アルゴリズムを直接実装することで、GoでのHTMLパースの基本を提供します。この低レベルのアプローチは強力ですが、複雑なタスクには冗長になることがあります。

対照的に、jQueryに触発されたサードパーティの `goquery` パッケージは、DOMの操作とトラバースを簡略化する高レベルのインターフェースを提供します。これにより、開発者は、要素の選択、属性の抽出、コンテンツの操作などのタスクに対して、簡潔かつ表現豊かなコードを書くことができます。

しかし、`goquery` の便利さは、追加の依存関係と、その抽象化層による潜在的なパフォーマンスの低下というコストを伴います。`net/html` と `goquery`（またはその他のパースライブラリ）のどちらを選択するかは、プロジェクトの具体的な要件、たとえばパフォーマンスの最適化や使いやすさの必要性によって異なります。

歴史的に見ると、GoでのHTMLパースは、基本的な文字列操作から洗練されたDOMツリー操作へと進化し、言語の成長するエコシステムとコミュニティによる強力なウェブスクレイピング及びデータ抽出ツールへの需要を反映しています。ネイティブ機能にもかかわらず、`goquery` などのサードパーティライブラリの普及は、Goコミュニティがモジュール式の再利用可能なコードを好むことを示しています。しかし、パフォーマンスが重要なアプリケーションでは、プログラマーは `net/html` パッケージを好むか、または単純なパースタスクに正規表現を使うこともありますが、正規表現によるHTMLパースの固有のリスクと限界を念頭に置く必要があります。
