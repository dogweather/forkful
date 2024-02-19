---
aliases:
- /ja/javascript/sending-an-http-request-with-basic-authentication/
date: 2024-01-20 18:01:56.162481-07:00
description: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u57FA\u672C\u8A8D\u8A3C\u3067\
  \u9001\u308B\u3068\u306F\u3001\u30E6\u30FC\u30B6\u30FC\u540D\u3068\u30D1\u30B9\u30EF\
  \u30FC\u30C9\u3092\u4F7F\u3063\u3066\u5B89\u5168\u306B\u60C5\u5831\u3092\u30B5\u30FC\
  \u30D0\u30FC\u306B\u9001\u4FE1\u3059\u308B\u3053\u3068\u3067\u3059\u3002\u3053\u308C\
  \u3092\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u30C7\u30FC\u30BF\u3092\u4FDD\u8B77\
  \u3057\u3001\u6A29\u9650\u306E\u3042\u308B\u30E6\u30FC\u30B6\u30FC\u3060\u3051\u304C\
  \u30A2\u30AF\u30BB\u30B9\u3067\u304D\u308B\u3088\u3046\u306B\u3059\u308B\u305F\u3081\
  \u306B\u884C\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:55.269102
model: gpt-4-1106-preview
summary: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u57FA\u672C\u8A8D\u8A3C\u3067\u9001\
  \u308B\u3068\u306F\u3001\u30E6\u30FC\u30B6\u30FC\u540D\u3068\u30D1\u30B9\u30EF\u30FC\
  \u30C9\u3092\u4F7F\u3063\u3066\u5B89\u5168\u306B\u60C5\u5831\u3092\u30B5\u30FC\u30D0\
  \u30FC\u306B\u9001\u4FE1\u3059\u308B\u3053\u3068\u3067\u3059\u3002\u3053\u308C\u3092\
  \u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u30C7\u30FC\u30BF\u3092\u4FDD\u8B77\u3057\
  \u3001\u6A29\u9650\u306E\u3042\u308B\u30E6\u30FC\u30B6\u30FC\u3060\u3051\u304C\u30A2\
  \u30AF\u30BB\u30B9\u3067\u304D\u308B\u3088\u3046\u306B\u3059\u308B\u305F\u3081\u306B\
  \u884C\u3044\u307E\u3059\u3002"
title: "\u57FA\u672C\u8A8D\u8A3C\u3092\u4F7F\u7528\u3057\u305FHTTP\u30EA\u30AF\u30A8\
  \u30B9\u30C8\u306E\u9001\u4FE1"
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
