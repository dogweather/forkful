---
title:                "ウェブページのダウンロード"
html_title:           "TypeScript: ウェブページのダウンロード"
simple_title:         "ウェブページのダウンロード"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## なぜ

Webページをダウンロードする理由はさまざまですが、一般的にはオフラインで閲覧したいか、データを分析したいからです。

## 使い方

```TypeScript
import * as request from 'request';

request('https://www.example.com', (error, response, body) => {
    console.log(body);
});
```

上記のように、requestモジュールを使用してウェブページをダウンロードすることができます。その後、bodyにウェブページの内容が格納され、コンソールに表示されます。

## 詳細を深く

ウェブページをダウンロードする際には、リクエストを送信し、レスポンスを受け取る必要があります。一般的には、HTTPリクエストを使用してウェブサーバーにリクエストを送信し、レスポンスを受け取ります。このプロセスは、ネットワーク通信やHTMLの解析など、多数のステップで構成されています。

## 参考

- requestモジュール: https://www.npmjs.com/package/request
- HTTPリクエスト: https://developer.mozilla.org/ja/docs/Web/HTTP/Overview