---
date: 2024-01-20 18:01:56.162481-07:00
description: "How to: (\u65B9\u6CD5) \u3053\u308C\u3067\u3001\u8A8D\u8A3C\u304C\u5FC5\
  \u8981\u306A\u30A8\u30F3\u30C9\u30DD\u30A4\u30F3\u30C8\u306B\u5BFE\u3057\u3066HTTP\
  \ GET\u30EA\u30AF\u30A8\u30B9\u30C8\u304C\u9001\u3089\u308C\u3001\u5FDC\u7B54\u304C\
  \u30B3\u30F3\u30BD\u30FC\u30EB\u306B\u8868\u793A\u3055\u308C\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.464455-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) \u3053\u308C\u3067\u3001\u8A8D\u8A3C\u304C\u5FC5\u8981\u306A\
  \u30A8\u30F3\u30C9\u30DD\u30A4\u30F3\u30C8\u306B\u5BFE\u3057\u3066HTTP GET\u30EA\
  \u30AF\u30A8\u30B9\u30C8\u304C\u9001\u3089\u308C\u3001\u5FDC\u7B54\u304C\u30B3\u30F3\
  \u30BD\u30FC\u30EB\u306B\u8868\u793A\u3055\u308C\u307E\u3059\u3002"
title: "\u57FA\u672C\u8A8D\u8A3C\u3092\u4F7F\u7528\u3057\u305FHTTP\u30EA\u30AF\u30A8\
  \u30B9\u30C8\u306E\u9001\u4FE1"
weight: 45
---

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
