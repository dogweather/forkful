---
title:                "HTMLの解析"
html_title:           "Go: HTMLの解析"
simple_title:         "HTMLの解析"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/parsing-html.md"
---

{{< edit_this_page >}}

## なぜ

HTML のパースを行うのか？
アプリケーションでウェブサイトにアクセスしデータを取得する際に、HTML を解析して必要な情報を抽出する必要があります。Go はそのようなタスクに最適な言語であり、効率的かつ簡潔なコードを書くことができます。

## 手順

```Go
func main() {
    // ウェブサイトからデータを取得するためのリクエストを作成
    res, err := http.Get("https://example.com")

    // エラー処理
    if err != nil {
        log.Fatal(err)
    }

    // レスポンスの Body を読み取る
    defer res.Body.Close()
    body, err := ioutil.ReadAll(res.Body)

    // エラー処理
    if err != nil {
        log.Fatal(err)
    }

    // HTML をパースする
    doc, err := html.Parse(strings.NewReader(string(body)))

    // エラー処理
    if err != nil {
        log.Fatal(err)
    }

    // マッチするタグを抽出する
    match := getElementsByTag(doc, "h1")[0] // h1 タグの最初の要素を抽出
    fmt.Println(match.FirstChild.Data)     // h1 タグのテキストを出力
}

// 指定されたタグの要素を取得する
func getElementsByTag(n *html.Node, tag string) []*html.Node {
    var elements []*html.Node

    // 子ノードをループして指定されたタグの要素を抽出する
    for c := n.FirstChild; c != nil; c = c.NextSibling {
        if c.Type == html.ElementNode && c.Data == tag {
            elements = append(elements, c)
        }
        elements = append(elements, getElementsByTag(c, tag)...)
    }

    return elements
}
```

出力:
```
Hello, world!
```

## ディープダイブ

Go 言語では、`html` パッケージを使用して HTML をパースします。このパッケージには、HTML ドキュメントをノードツリーに変換する `html.Parse` 関数があります。そして、ノードツリーを操作することで、特定の要素や属性を取得することができます。

また、`golang.org/x/net/html` パッケージを使用することで、HTML をより柔軟にパースすることができます。このパッケージには、CSS セレクタを使用した要素の抽出や、HTML ドキュメントのシリアライズなどの機能があります。

## See Also

- [Effective Go](https://golang.org/doc/effective_go.html)
- [html パッケージ](https://golang.org/pkg/html/)
- [golang.org/x/net/html パッケージ](https://godoc.org/golang.org/x/net/html)