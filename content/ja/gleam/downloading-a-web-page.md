---
title:                "Gleam: 「ウェブページのダウンロード」"
simple_title:         "「ウェブページのダウンロード」"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## なぜダウンロードするのか

Webサイトのコンテンツを解析したり、データを収集したりするために、多くの開発者がWebページをダウンロードするでしょう。Gleam言語を使えば、簡単にWebページをダウンロードすることができます。

## ダウンロードする方法

まず、Gleamプロジェクトを新しく作成します。その中で、以下のコードを実行します。

```Gleam
use http

let url = "https://example.com"
let page = http.get(url)
```

上記のコードでは、`http`モジュールをインポートし、ダウンロードするWebページのURLを定義しています。そして、`http.get`関数を使ってページをダウンロードしています。このコードを実行すると、指定したURLのWebページがダウンロードされ、`page`変数に保存されます。

もし、特定のリクエストヘッダーやクエリパラメーターを追加したい場合は、`http.get`関数の第二引数としてオプションのオブジェクトを渡すことができます。また、GET以外のHTTPメソッドを使いたい場合は、代わりに`http.request`関数を使うことができます。

## 深く掘り下げる

`page`変数には、ダウンロードしたWebページのHTMLソースコードが含まれています。このコードを解析したり、必要な情報を抽出したりすることで、より高度なデータ収集やスクレイピングを行うことができます。また、`http.get`関数や`http.request`関数を使うことで、その他のHTTPリクエストを実行することもできます。

## さらに参考になる情報

- [Gleam 公式ドキュメント](https://gleam.run/documentation/)
- [Gleam - HTTP Module](https://gleam.run/documentation/std-lib-http/)
- [Gleam - HTTP Request Functions](https://gleam.run/documentation/std-lib-http/functions/)