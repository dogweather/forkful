---
title:                "ウェブページのダウンロード"
html_title:           "Bash: ウェブページのダウンロード"
simple_title:         "ウェブページのダウンロード"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# ウェブページのダウンロード: Goがどのようにそれを行うか

## 何 & なぜ?

ウェブページのダウンロードとは、特定のURLからデータを取得し、ローカル環境で利用可能にすることです。プログラマーがこれを行う理由は、サイトの情報を収集したり、オフラインで利用するためです。

## 実装方法:

Go言語でウェブページをダウンロードする一つの単純な方法は、`net/http`パッケージを使用することです。

```Go
package main

import (
    "io"
    "net/http"
    "os"
)

func main() {
    response, err := http.Get("http://example.com")
    if err != nil {
        panic(err)
    }
    defer response.Body.Close()

    file, err := os.Create("output.html")
    if err != nil {
        panic(err)
    }
    defer file.Close()

    _, err = io.Copy(file, response.Body)
    if err != nil {
        panic(err)
    }
}
```

このコードは、まず"http://example.com"にHTTP GETリクエストを行います。次に、レスポンスボディを"output.html"という名前のファイルに保存します。

## ディープダイブ:

歴史的な文脈：ウェブスクレイピング（ウェブページのダウンロードを含む）は、インターネットが主流になってからずっと存在しています。それは情報の取得と共有を容易にし、今日のデータ駆動型世界の重要な一部となっています。

代替手段：Go以外の多くの言語、例えばPythonやJavaScriptでは、豊富なライブラリとツールがウェブスクレイピングを補助しています。しかし、Goはそのパフォーマンスと効率性で注目を浴びています。

実装詳細：このコードは、`http.Get()`関数を使用してHTTPリクエストを送信します。その後、`os.Create()`で新しいファイルを作成し、`io.Copy()`でレスポンスデータをそのファイルにコピーします。

## 参考資料:

- Go言語の公式ドキュメンテーション(https://golang.org/doc/)
- Goによるウェブスクレイピング(https://edmundmartin.com/writing-a-web-crawler-in-golang/)
- httpプロトコルについての詳細(https://developer.mozilla.org/ja/docs/Web/HTTP)