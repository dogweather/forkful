---
title:                "HTTPリクエストの送信"
html_title:           "Javascript: HTTPリクエストの送信"
simple_title:         "HTTPリクエストの送信"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## なぜ
HTTPリクエストを送信する理由は、ウェブ開発において欠かせないものです。ウェブサイトやアプリケーションは、クライアントとサーバーがデータのやりとりをすることで機能します。HTTPリクエストは、そのデータのやりとりを可能にする重要な手段です。

## 方法
HTTPリクエストを送信する方法はいくつかありますが、ここではJavascriptを使用した方法を紹介します。まずは、`XMLHttpRequest`オブジェクトを使用してリクエストを作成します。次に、`open()`メソッドを使用してリクエストの種類や送信先のURLを指定し、`send()`メソッドでリクエストを送信します。例えば、以下のようになります。

```Javascript
const request = new XMLHttpRequest();
request.open('GET', 'https://example.com/api/users');
request.send();
```

上記の例では、`GET`メソッドを使用して`https://example.com/api/users`へのリクエストを送信しています。`GET`以外にも、`POST`や`PUT`などのメソッドを使用することもできます。また、応答を受け取るためには、`onload`イベントを使用したり、`send()`メソッドの引数としてコールバック関数を設定することもできます。

## 深堀り
HTTPリクエストには、さまざまな種類があります。例えば、単純なGETリクエストの他にも、認証付きのリクエストやフォームデータを含むPOSTリクエストなどがあります。また、リクエストのヘッダーには、要求内容やクライアントの情報などを含めることもできます。さらに、HTTPリクエストの内容を確認するために、開発者ツールやデバッガーを使用することもできます。

See Also
- [XMLHttpRequest - MDN Web Docs](https://developer.mozilla.org/ja/docs/Web/API/XMLHttpRequest)
- [HTTP リクエストを送信する方法 - MDN Web Docs](https://developer.mozilla.org/ja/docs/Learn/JavaScript/Client-side_web_APIs/Fetching_data)