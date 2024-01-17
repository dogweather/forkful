---
title:                "「HTTPリクエストの送信」"
html_title:           "Javascript: 「HTTPリクエストの送信」"
simple_title:         "「HTTPリクエストの送信」"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 何 & なぜ?

HTTPリクエストを送信することとは、Webブラウザやアプリケーションがサーバーに情報を要求することを意味します。プログラマーがHTTPリクエストを送信する目的は、ウェブサイトやアプリケーションを動的に作成するために必要なデータや機能を取得することです。

## 方法:

以下のようなコードを使用して、JavascriptでHTTPリクエストを送信することができます。

```
var xhr = new XMLHttpRequest();
xhr.open('GET', 'https://example.com/api/data', true);
xhr.send();

```

上記のコードでは、新しいXMLHttpRequestオブジェクトを作成し、GETリクエストを送信しています。また、リクエストURLや非同期処理の使用方法も示しています。

## 深堀り:

HTTPリクエストには、HTMLフォームやAjaxなどの代替手段も存在します。また、リクエストヘッダーやレスポンスコードなど、詳細な実装方法や制御方法もあります。HTTPリクエストは、Web開発において重要な概念であり、適切に理解することが必要です。

## 関連リンク:

- [HTTP リクエストとは](https://developer.mozilla.org/ja/docs/Web/HTTP/Overview#request_messages)
- [XMLHttpRequest オブジェクト](https://developer.mozilla.org/ja/docs/Web/API/XMLHttpRequest)
- [AJAX 入門](https://www.w3schools.com/xml/ajax_intro.asp)