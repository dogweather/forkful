---
title:                "「HTMLの解析」"
html_title:           "Go: 「HTMLの解析」"
simple_title:         "「HTMLの解析」"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/parsing-html.md"
---

{{< edit_this_page >}}

## 何＆なぜ？

HTMLのパースとは、webページの構造や内容を読み取ることです。プログラマーがHTMLをパースする理由は、webスクレイピングやデータの収集、webクローラーの作成など様々です。

## 方法：

```Go
func main() {
    // HTMLのパース方法
    var doc *html.Node
    resp, err := http.Get("https://example.com")
    if err != nil {
        // HTTPリクエストが失敗した場合の処理
    }
    // HTTPレスポンスのBodyをクローズ
    defer resp.Body.Close()
    doc, err = html.Parse(resp.Body)
    if err != nil {
        // HTMLのパースが失敗した場合の処理
    }
    // パースしたHTMLを取得する
    fmt.Println(doc)
}
```

出力：

```Go
<body>
    <h1>This is an example page</h1>
    <p>Example paragraph.</p>
</body>
```

## 深堀り：

HTMLのパースは、1993年に発明されたプログラミング言語である、メタラボ社のSGMLに基づいているXMLから派生したものです。Go言語では、標準ライブラリの"golang.org/x/net/html"パッケージを使用することでHTMLをパースすることができます。他の代替手段としては、サードパーティ製のライブラリやフレームワークを使用することもできます。

## 関連リンク：

- [Golang.org/x/net/htmlパッケージドキュメント](https://pkg.go.dev/golang.org/x/net/html)
- [Go言語ドキュメント](https://golang.org/doc/)