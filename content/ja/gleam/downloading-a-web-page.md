---
title:                "ウェブページのダウンロード"
html_title:           "Bash: ウェブページのダウンロード"
simple_title:         "ウェブページのダウンロード"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/downloading-a-web-page.md"
---

{{< edit_this_page >}}

**## 何となぜ？**

ウェブページのダウンロードとは、ウェブページの情報を自分のデバイスに保存することです。プログラマはこれを行うことで、オフラインでも情報を参照したり、データ解析のために使用したりします。

**## どのようにして：**

Gleamでウェブページをダウンロードする方法を見てみましょう。以下にそのコードを示します。

```gleam
import gleam/http.{get, start_link}

fn main() {
  let _ = start_link(default_http_client)

  case get("https://example.com") {
    Ok(response) ->
      response.body
        |> Result.unwrap
        |> IO.println

    Error(err) ->
      err
        |> error.to_string
        |> IO.println
  }
}
```

このコードは、Gleamのhttpライブラリを使用してウェブサイトから情報を取得します。エラーが発生した場合、それはコンソールにプリントされます。

**## 深層ダイブ：**

ウェブページのダウンロードは、インターネットが誕生して以降、一般的な活動となりました。初期のウェブブラウザにより、これが可能になりました。現在、これはAPIを介してデータを取得し、解析するための主要な手段となっています。

ダウンロードの代替手段としては、APIを用いる方法や、ウェブスクレイピングがあります。APIは通常、データの提供を目的として作られ、より効率的にデータを取得できます。ウェブスクレイピングは、APIが提供されていない場合や、特定の情報が必要な場合に有用です。

Gleamでの実装については、上記のコードを参照してください。HTTPリクエストとレスポンスを取り扱うためのライブラリが用意されています。エラーハンドリングも含まれています。

**## 参考リンク：**

- Gleamの公式ドキュメント: [https://gleam.run/book](https://gleam.run/book)
- ウェブスクレイピングのチュートリアル: [https://realpython.com/beautiful-soup-web-scraper-python/](https://realpython.com/beautiful-soup-web-scraper-python/)