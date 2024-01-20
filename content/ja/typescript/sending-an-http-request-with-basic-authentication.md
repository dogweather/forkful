---
title:                "基本認証を使用してhttpリクエストを送信する"
html_title:           "C#: 基本認証を使用してhttpリクエストを送信する"
simple_title:         "基本認証を使用してhttpリクエストを送信する"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 何となぜ?
HTTPリクエストをベーシック認証で送信するとは、ユーザー名とパスワードを用いてサーバーへのアクセスを証明するための方法です。これは、含まれているデータの保護と、特定のウェブページやAPIエンドポイントに対する制限付きアクセスを可能にするために行われます。

## 手順：
TypeScriptでのベーシック認証を使用したHTTPリクエストの送信は、以下のようになります。

```TypeScript
import axios from 'axios';

const sendRequest = async () => {
  const response = await axios.get('https://example.com', {
    auth: {
      username: 'your-username',
      password: 'your-password'
    }
  });
  console.log(response.data);
};

sendRequest();
```

これで、指定したユーザー名とパスワードでサーバーにHTTPリクエストを送信し、その結果をコンソールに出力します。

## ディープダイブ
ベーシック認証は、HTTP/1.0時代から存在し、シンプルさと広い対応範囲が強みです。しかし、今日ではより高度な手法、例えばBearerトークンやOAuthが一般的に使われています。ベーシック認証では、ユーザー名とパスワードがBase64でエンコードされ送信されますが、暗号化されていないため、HTTPSを用いて通信することが必須となります。

## 関連情報
詳細な情報とベーシック認証の代替手法については、以下のリンクをご覧ください。

1. [MDN Web Docs (Basic authentication)](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#Basic_authentication)
2. [Bearer Token](https://jwt.io/introduction/)
3. [OAuth 2.0](https://oauth.net/2/)