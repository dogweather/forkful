---
date: 2024-01-20 18:03:11.828883-07:00
description: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306B\u30D9\u30FC\u30B7\u30C3\u30AF\
  \u8A8D\u8A3C\u3092\u3064\u3051\u308B\u3068\u306F\u3001\u30E6\u30FC\u30B6\u30FC\u540D\
  \u3068\u30D1\u30B9\u30EF\u30FC\u30C9\u3092\u4F7F\u3063\u3066\u30B5\u30FC\u30D0\u30FC\
  \u306B\u5B89\u5168\u306B\u30A2\u30AF\u30BB\u30B9\u3059\u308B\u3053\u3068\u3002\u3053\
  \u308C\u3092\u30D7\u30ED\u30B0\u30E9\u30DE\u304C\u884C\u3046\u7406\u7531\u306F\u3001\
  \u4FDD\u8B77\u3055\u308C\u305F\u30EA\u30BD\u30FC\u30B9\u3078\u306E\u5B89\u5168\u306A\
  \u30A2\u30AF\u30BB\u30B9\u3092\u63D0\u4F9B\u3059\u308B\u305F\u3081\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:41.757172-06:00'
model: gpt-4-1106-preview
summary: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306B\u30D9\u30FC\u30B7\u30C3\u30AF\u8A8D\
  \u8A3C\u3092\u3064\u3051\u308B\u3068\u306F\u3001\u30E6\u30FC\u30B6\u30FC\u540D\u3068\
  \u30D1\u30B9\u30EF\u30FC\u30C9\u3092\u4F7F\u3063\u3066\u30B5\u30FC\u30D0\u30FC\u306B\
  \u5B89\u5168\u306B\u30A2\u30AF\u30BB\u30B9\u3059\u308B\u3053\u3068\u3002\u3053\u308C\
  \u3092\u30D7\u30ED\u30B0\u30E9\u30DE\u304C\u884C\u3046\u7406\u7531\u306F\u3001\u4FDD\
  \u8B77\u3055\u308C\u305F\u30EA\u30BD\u30FC\u30B9\u3078\u306E\u5B89\u5168\u306A\u30A2\
  \u30AF\u30BB\u30B9\u3092\u63D0\u4F9B\u3059\u308B\u305F\u3081\u3002."
title: "\u57FA\u672C\u8A8D\u8A3C\u3092\u4F7F\u7528\u3057\u305FHTTP\u30EA\u30AF\u30A8\
  \u30B9\u30C8\u306E\u9001\u4FE1"
weight: 45
---

## How to: (方法)
```TypeScript
import axios from 'axios';

async function fetchWithBasicAuth(url: string, username: string, password: string) {
  try {
    const response = await axios.get(url, {
      auth: {
        username,
        password
      }
    });
    console.log(response.data);
  } catch (error) {
    console.error('Authentication failed:', error);
  }
}

// 例: 'https://api.example.com/data' へのベーシック認証を使ったリクエスト
fetchWithBasicAuth('https://api.example.com/data', 'my_username', 'my_password');
```

Sample Output (サンプル出力):

```
{ "some": "data" }
```

Or, using Node.js built-in `https` module:

```TypeScript
import https from 'https';
import { Buffer } from 'buffer';

function fetchWithBasicAuth(url: string, username: string, password: string) {
  const encodedAuth = Buffer.from(`${username}:${password}`).toString('base64');
  const options = {
    headers: {
      'Authorization': `Basic ${encodedAuth}`
    }
  };

  https.get(url, options, (res) => {
    let data = '';
    res.on('data', (chunk) => data += chunk);
    res.on('end', () => console.log(data));
  }).on('error', (err) => {
    console.error('Authentication failed:', err);
  });
}

fetchWithBasicAuth('https://api.example.com/secure-data', 'my_username', 'my_password');
```

## Deep Dive (詳細情報)
ベーシック認証はHTTPプロトコルで最も単純な認証の形式。RFC 7617に定義され、初期のウェブから存在。安全でないことが知られ、今はTLS(HTTPS)と併用される。ベーシック認識以外に、OAuthやBearer認証などのより安全な方法もある。

内部で、ユーザー名とパスワードはコロン（:）で連結され、Base64でエンコードされて`Authorization`ヘッダーにセットされる。しかし、Base64は暗号化ではなくエンコード手法なので、セキュリティのためHTTPSが必須。

TypeScriptでは`axios`のようなライブラリやNode.jsの`http`/`https`モジュールを使って実装可能。上記の例では、最初に`axios`を使ったシンプルな例を紹介。次にノードの組み込みモジュールを使用した処理方法を説明。

## See Also (関連情報)
- Axios GitHub repository: [https://github.com/axios/axios](https://github.com/axios/axios)
- HTTP Basic Access Authentication on MDN: [https://developer.mozilla.org/docs/Web/HTTP/Authentication](https://developer.mozilla.org/docs/Web/HTTP/Authentication)
- RFC 7617, The 'Basic' HTTP Authentication Scheme: [https://tools.ietf.org/html/rfc7617](https://tools.ietf.org/html/rfc7617)
- Node.js `https` module: [https://nodejs.org/api/https.html](https://nodejs.org/api/https.html)
