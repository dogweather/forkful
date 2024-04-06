---
date: 2024-01-20 18:03:11.828883-07:00
description: "How to: (\u65B9\u6CD5) \u30D9\u30FC\u30B7\u30C3\u30AF\u8A8D\u8A3C\u306F\
  HTTP\u30D7\u30ED\u30C8\u30B3\u30EB\u3067\u6700\u3082\u5358\u7D14\u306A\u8A8D\u8A3C\
  \u306E\u5F62\u5F0F\u3002RFC 7617\u306B\u5B9A\u7FA9\u3055\u308C\u3001\u521D\u671F\
  \u306E\u30A6\u30A7\u30D6\u304B\u3089\u5B58\u5728\u3002\u5B89\u5168\u3067\u306A\u3044\
  \u3053\u3068\u304C\u77E5\u3089\u308C\u3001\u4ECA\u306FTLS(HTTPS)\u3068\u4F75\u7528\
  \u3055\u308C\u308B\u3002\u30D9\u30FC\u30B7\u30C3\u30AF\u8A8D\u8B58\u4EE5\u5916\u306B\
  \u3001OAuth\u3084Bearer\u8A8D\u8A3C\u306A\u3069\u306E\u3088\u308A\u5B89\u5168\u306A\
  \u65B9\u6CD5\u3082\u3042\u308B\u3002\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:50:55.720931-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) \u30D9\u30FC\u30B7\u30C3\u30AF\u8A8D\u8A3C\u306FHTTP\u30D7\
  \u30ED\u30C8\u30B3\u30EB\u3067\u6700\u3082\u5358\u7D14\u306A\u8A8D\u8A3C\u306E\u5F62\
  \u5F0F\u3002RFC 7617\u306B\u5B9A\u7FA9\u3055\u308C\u3001\u521D\u671F\u306E\u30A6\
  \u30A7\u30D6\u304B\u3089\u5B58\u5728\u3002\u5B89\u5168\u3067\u306A\u3044\u3053\u3068\
  \u304C\u77E5\u3089\u308C\u3001\u4ECA\u306FTLS(HTTPS)\u3068\u4F75\u7528\u3055\u308C\
  \u308B\u3002\u30D9\u30FC\u30B7\u30C3\u30AF\u8A8D\u8B58\u4EE5\u5916\u306B\u3001OAuth\u3084\
  Bearer\u8A8D\u8A3C\u306A\u3069\u306E\u3088\u308A\u5B89\u5168\u306A\u65B9\u6CD5\u3082\
  \u3042\u308B\u3002"
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
