---
title:                "ウェブページのダウンロード"
html_title:           "Javascript: ウェブページのダウンロード"
simple_title:         "ウェブページのダウンロード"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 何 & なぜ？

ウェブページのダウンロードとは、インターネット上の情報を自分のコンピューターに保存することです。プログラマーがこれを行う理由は、ウェブページの内容をオフラインで参照するために必要な場合があるからです。

## 方法：

```Javascript
const request = require('request');
request('https://www.example.com', function(err, res, body) {
    console.log(body);
});
```

このコードはNode.jsを使用して、指定したウェブページをダウンロードし、ターミナル上に表示します。データを取得するために ```request``` パッケージを使用し、サーバーからのレスポンスを取得するためにコールバック関数を使用しています。

## 深堀り：

ウェブページをダウンロードするプロセスは、1990年代初めに開発されたHTTPプロトコルに依存しています。以前は、ウェブページのダウンロードは手動で行われましたが、今ではプログラマーは自動化したスクリプトを使用して、大量のページをダウンロードすることができます。代替手段として、ブラウザーの機能を使用してウェブページをダウンロードすることもできます。ウェブページのダウンロードは、ウェブスクレイピングやデータ収集など、様々な用途に使用されています。

## 関連リンク：

- [Node.js requestパッケージ](https://www.npmjs.com/package/request)
- [ブラウザーでのウェブページのダウンロード](https://www.top10bestwebsitebuilders.com/how-to-download-a-website)
- [ウェブスクレイピングの活用例](https://www.webharvy.com/articles/web-scraping-examples.html)