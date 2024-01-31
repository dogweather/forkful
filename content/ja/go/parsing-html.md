---
title:                "HTMLの解析"
date:                  2024-01-20T15:32:06.809864-07:00
html_title:           "Arduino: HTMLの解析"
simple_title:         "HTMLの解析"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? / 何となぜ？

HTMLの解析（Parsing HTML）は、ウェブページの構造を理解したり、特定のデータを取得したりするために行います。プログラマーは通常、Webスクレイピングやデータマイニング、自動化されたテストなどにこの手法を活用します。

## How to / 方法

Go言語でHTMLを解析するには、標準ライブラリに含まれる`"net/html"`パッケージを使用します。以下のサンプルコードでは、HTMLドキュメントからタイトルを抽出しています。

```go
package main

import (
	"fmt"
	"strings"
	"golang.org/x/net/html"
)

func main() {
	h := `<html><head><title>サンプルページ</title></head><body><p>こんにちは、世界！</p></body></html>`
	doc, err := html.Parse(strings.NewReader(h))
	if err != nil {
		panic(fmt.Sprintf("HTML parse error: %v", err))
	}

	var f func(*html.Node)
	f = func(n *html.Node) {
		if n.Type == html.ElementNode && n.Data == "title" {
			fmt.Println(n.FirstChild.Data)
		}
		for c := n.FirstChild; c != nil; c = c.NextSibling {
			f(c)
		}
	}

	f(doc)
}
```

このコードを実行すると、以下の出力が得られます。

```
サンプルページ
```

## Deep Dive / 詳細な解析

HTMLの解析は、Webの成立初期から必要とされてきました。初期の解析器は簡単な正規表現に頼ることも多かったですが、今日ではDOM（Document Object Model）に基づく解析が一般的です。`net/html`パッケージはGo言語におけるHTML解析のためのロバストなツールです。`net/html`は、効率的なトークン化と構文解析のために、内部的にトークンベースのパーサーを使用しています。

替わりに、「goquery」のようなサードパーティのライブラリを利用することもあります。これらのライブラリは、jQueryのような構文を使用して、柔軟かつ便利なDOM操作を可能にすることが特徴です。

## See Also / 関連する情報

- Go言語の公式ドキュメント: [net/html package](https://pkg.go.dev/golang.org/x/net/html)
- クエリベースの操作を簡単にする `goquery`: [GoQuery package](https://github.com/PuerkitoBio/goquery)
- HTMLとDOMについての詳細情報: [Mozilla Developer Network - DOM](https://developer.mozilla.org/en-US/docs/Web/API/Document_Object_Model)
