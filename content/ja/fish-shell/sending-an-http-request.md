---
title:                "「httpリクエストを送信する」"
html_title:           "Fish Shell: 「httpリクエストを送信する」"
simple_title:         "「httpリクエストを送信する」"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## なぜ

HTTPリクエストを送信するメリットはたくさんあります。もしデータをサーバーに送信したい場合や、他のウェブサイトから情報を取得したい場合に、HTTPリクエストを使用することができます。本記事では、どのようにFish Shellを使用して簡単にHTTPリクエストを送信するかを解説します。

## 使い方

まずは、Fish Shellを使ってHTTPリクエストを送信するために必要なライブラリをインストールしましょう。

```Fish Shell
npm install node-fetch --save
```

次に、以下のコードを使用して、サーバーにHTTPリクエストを送信することができます。

```Fish Shell
var fetchUrl = require("fetch").fetchUrl;

fetchUrl("https://www.example.com", function(error, meta, body) {
  console.log(body.toString());
});
```

これで、コンソールにサーバーから返ってきたデータが表示されるはずです。HTTPリクエストをカスタマイズしたい場合は、以下のコードを使用することでヘッダーやクエリパラメーターを追加することができます。

```Fish Shell
var fetchUrl = require("fetch").fetchUrl;
var queryString = require("query-string");

var queryStr = queryString.stringify({
  name: "John",
  age: 25
});

var url = "https://www.example.com?" + queryStr;

fetchUrl(url, function(error, meta, body) {
  console.log(body.toString());
});
```

以上で、Fish Shellを使用して簡単にHTTPリクエストを送信することができます。

## ディープダイブ

Fish Shellを使用してHTTPリクエストを送信する際には、サードパーティライブラリを使用することをおすすめします。今回は、「node-fetch」というライブラリを使用しましたが、他にも「axios」や「request」などのライブラリを使用することができます。また、コールバック関数の代わりにプロミス（Promise）を使用することで、コードをよりシンプルかつ読みやすくすることもできます。

## おすすめリンク

- [node-fetch - npm](https://www.npmjs.com/package/node-fetch)
- [axios - npm](https://www.npmjs.com/package/axios)
- [request - npm](https://www.npmjs.com/package/request)
- [コールバック関数とプロミス（Promise）の違い](https://qiita.com/saxxiota/items/4614eb077bf481481372)