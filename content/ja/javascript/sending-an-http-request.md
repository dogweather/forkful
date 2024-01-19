---
title:                "HTTPリクエストの送信"
html_title:           "Bash: HTTPリクエストの送信"
simple_title:         "HTTPリクエストの送信"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 何となぜ？

HTTPリクエストの送信とは、サーバーからデータを要求する行為です。プログラマーはこれを行うことで、ウェブページのデータを取得したり、データベースとのやりとりを行ったりします。

## 使い方：

以下のコードブロックでは、fetch APIを使用してHTTPリクエストを送信し、そのレスポンスをログに出力します。

```Javascript
fetch('https://api.github.com/users/octocat')
  .then(response => response.json())
  .then(data => console.log(data))
  .catch((error) => console.log('Error:', error));
```
実行結果としては、指定したURLから得られたJSONデータが出力されます。

## 深掘り：

史上初のHTTPリクエストは、1991年に行われました。現在では多くの方法で行われています。fetch APIは現代のブラウザに組み込まれており、プロミスベースのインターフェイスでHTTPリクエストを送信するための方法です。古いブラウザでは、これと同様の操作を行うためにXMLHttpRequestオブジェクトが使われることがあります。

実行時にURLが存在しない場合やサーバーからの応答がない場合、fetchはResolveの代わりにRejectを返しません。代わりに、okステータスを確認してエラーハンドリングを行います。

## 参考資料：

さらなる学習のためには、以下のツールやリソースを参考にしてください：

- [MDNのFetch APIドキュメント](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API)
- [MDNのHTTPリクエスト詳細](https://developer.mozilla.org/en-US/docs/Web/HTTP/Messages)
- [Node.jsでHTTPリクエストを行う方法](https://nodejs.dev/learn/making-http-requests-with-nodejs)