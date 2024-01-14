---
title:                "TypeScript: ベーシック認証を使用してhttpリクエストを送信する"
simple_title:         "ベーシック認証を使用してhttpリクエストを送信する"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## なぜ

HTTPリクエストを基本認証と共に送信することの重要性について説明します。

基本認証を使うと、サーバー側でユーザーの認証を簡単に行うことができます。また、セキュリティやアクセス制限を設定することができるため、データやサービスの保護にも役立ちます。

## 使い方

基本認証を使用してHTTPリクエストを送信する方法について、コーディングの例と出力のサンプルを以下のコードブロックで説明します。

```TypeScript 
import { request } from 'http';

const options = {
  hostname: 'example.com',
  port: 80,
  path: '/login',
  method: 'POST',
  auth: 'username:password'
}

const req = request(options, (res) => {
  console.log(`ステータスコード: ${res.statusCode}`)
});

req.end();
```

この例では、インポートした`http`モジュールを使用し、基本認証の情報を指定した後、リクエストを送信します。そして、応答コードをコンソールに表示します。

## ディープダイブ

基本認証を含むHTTPリクエストをより詳細に理解するために、以下のような要素についても学ぶことができます。

- 基本認証のヘッダーとクレデンシャルの形式
- エラー処理とフロー制御の方法
- リダイレクトとリトライのサポート

これらの要素を理解することで、より柔軟に基本認証を使用したHTTPリクエストを扱えるようになります。

## 参考リンク

- [Node.jsで基本認証付きのHTTPリクエストを送信する方法](https://stackabuse.com/the-node-js-way-getting-to-know-http/)
- [Node.jsの基本認証モジュールのドキュメント](https://nodejs.org/api/http.html#http_http_request_options_callback)
- [基本認証についての詳細な説明](https://developer.mozilla.org/ja/docs/Web/HTTP/Authentication#Basic_authentication_scheme)