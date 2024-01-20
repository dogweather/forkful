---
title:                "基本認証を使用してhttpリクエストを送信する"
html_title:           "C#: 基本認証を使用してhttpリクエストを送信する"
simple_title:         "基本認証を使用してhttpリクエストを送信する"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 何となぜ？
HTTPリクエストの基本認証送信は、特定のエンドポイントへアクセス制限をかけるプロセスです。これにより、特定のユーザーのみがリソースへアクセスできるようになります。

## どうやるの？
基本的な認証においては、ユーザーネームとパスワードを `Authorization`ヘッダにエンコードして送信します。下記はその例です：

```Javascript
const https = require('https');
let username = 'user';
let password = 'pass';
let options = {
  hostname: 'example.com',
  port: 443,
  path: '/api/data',
  method: 'GET',
  headers: {
    'Authorization': 'Basic ' + Buffer.from(username + ":" + password).toString('base64')
  }
};
const req = https.request(options, res => {
  res.on('data', d => {
    process.stdout.write(d);
  });
});
req.end();
```

このシンプルなコードは `example.com`サイトのAPIエンドポイント `/api/data`に基本認証でGETリクエストを行います。

## ディープダイブ
基本認証はHTTPプロトコルの一部として1990年代初期に導入されました。しかし、テキストはbase64でエンコードされているだけで、実際には暗号化されていないため、安全性に欠けます。セキュリティを改善するためにトークンベースの認証（例えばOAuth）が推奨されます。

それぞれの実装は異なりますが、一般的なHTTPライブラリーでは `headers`オプション内に認証を行うための情報を含ませることが共通しています。

## 参考文献
- ベーシック認証の深い解説: https://developer.mozilla.org/ja/docs/Web/HTTP/Authentication
- より安全なOAuthの仕組み: https://oauth.net/
- ノードJSのHTTPSライブラリの詳細なドキュメンテーション: https://nodejs.org/api/https.html