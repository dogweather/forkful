---
title:                "基本認証を使用したHTTPリクエストの送信"
aliases: - /ja/typescript/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-20T18:03:11.828883-07:00
model:                 gpt-4-1106-preview
simple_title:         "基本認証を使用したHTTPリクエストの送信"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)

HTTPリクエストにベーシック認証をつけるとは、ユーザー名とパスワードを使ってサーバーに安全にアクセスすること。これをプログラマが行う理由は、保護されたリソースへの安全なアクセスを提供するため。

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
