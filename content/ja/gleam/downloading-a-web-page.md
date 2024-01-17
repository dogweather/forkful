---
title:                "ウェブページのダウンロード"
html_title:           "Gleam: ウェブページのダウンロード"
simple_title:         "ウェブページのダウンロード"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## なに？なんで？

ウェブページをダウンロードすることとは、インターネット上のコンテンツを自分のコンピューターに保存することを指します。プログラマーがこれを行う理由は、インターネット上のデータを使用して自分のアプリケーションを作ることや、特定の情報を収集するために必要だからです。

## 方法：

```Gleam id
  //ウェブページをダウンロードする方法
  let request = http.request(endpoint: "https://www.example.com")
  //レスポンスのボディを取得する
  let body = http.body(request)
  //取得したレスポンスをコンソールに印字する
  io.format("Response body: {}", [body])
```

## 詳しく見る：

インターネットが発達したことで、多くのプログラマーがウェブページをダウンロードすることを必要とするようになりました。これは、自分のアプリケーションでウェブサイトからデータを取得したり、特定の情報を収集するために欠かせない作業です。代替手段として、スクレイピングやクローリングといった方法もありますが、ウェブページを直接ダウンロードすることは最も一般的な方法です。

## 関連情報の確認：

- Gleam公式ドキュメント：https://gleam.run/
- ウェブページダウンロードの代替手段：https://techbriefers.com/alternatives-to-web-page-downloading
- ウェブスクレイピングの詳細：https://techbriefers.com/web-scraping