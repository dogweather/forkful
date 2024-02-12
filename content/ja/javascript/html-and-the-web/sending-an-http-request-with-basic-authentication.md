---
title:                "基本認証を使用したHTTPリクエストの送信"
aliases: - /ja/javascript/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-20T18:01:56.162481-07:00
model:                 gpt-4-1106-preview
simple_title:         "基本認証を使用したHTTPリクエストの送信"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
HTTPリクエストを基本認証で送るとは、ユーザー名とパスワードを使って安全に情報をサーバーに送信することです。これをプログラマーはデータを保護し、権限のあるユーザーだけがアクセスできるようにするために行います。

## How to: (方法)
```javascript
// Node.jsの`fetch`を使った基本認証付きHTTPリクエストの例
const fetch = require('node-fetch');
const base64 = require('base-64');

// ユーザー名とパスワードをエンコード
const username = 'user';
const password = 'pass';
const auth = 'Basic ' + base64.encode(username + ':' + password);

// リクエストオプションを設定
const requestOptions = {
  method: 'GET',
  headers: {
    'Authorization': auth
  }
};

// リクエストを送信
const url = 'https://example.com/data';
fetch(url, requestOptions)
  .then(response => response.json())
  .then(data => console.log(data))
  .catch(error => console.error('Error:', error));
```
これで、認証が必要なエンドポイントに対してHTTP GETリクエストが送られ、応答がコンソールに表示されます。

## Deep Dive (掘り下げ)
基本認証（Basic Authentication）とは、RFC 7617で定義されている認証の方法であり、初期のHTTPプロトコルから存在しています。リクエストヘッダーにユーザー名とパスワードを含めるシンプルな方法です。ただし、生のユーザー名とパスワードはBase64エンコードされて通信されるため、HTTPSを使うことで情報が暗号化されるまではセキュリティが低いことに注意が必要です。

代替手段としてOAuth、トークンベースの認証があり、よりセキュアで柔軟です。しかし、単純なシステムやレガシーなシステムでは今でも基本認証が使用されています。

実装については、`fetch` APIを使いましたが、Node.jsでは標準ではないため、`node-fetch`モジュールを利用する必要があります。ブラウザ環境では`fetch` APIが標準で利用できます。また、別の方法としてXMLHttpRequestを使うことも可能ですが、現代では`fetch`が推奨されています。

## See Also (関連情報)
- [MDN Web Docs - Authorization](https://developer.mozilla.org/ja/docs/Web/HTTP/Headers/Authorization)
- [Using Fetch - MDN Web Docs](https://developer.mozilla.org/ja/docs/Web/API/Fetch_API/Using_Fetch)
- [node-fetch on npm](https://www.npmjs.com/package/node-fetch)
- [RFC 7617 - The 'Basic' HTTP Authentication Scheme](https://tools.ietf.org/html/rfc7617)
