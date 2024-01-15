---
title:                "基本認証を用いたhttpリクエストの送信"
html_title:           "TypeScript: 基本認証を用いたhttpリクエストの送信"
simple_title:         "基本認証を用いたhttpリクエストの送信"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Why
HTTPリクエストを基本認証で送信する理由は、ウェブアプリケーションやAPIなど、二つの異なるシステム間でセキュリティを確保するためです。基本認証では、ユーザー名とパスワードを使用して認証を行い、認証が成功した場合のみリクエストが送信されます。

## How To
```TypeScript
import axios from 'axios';

// 例：基本認証を使用してAPIエンドポイントにGETリクエストを送信する
axios.get('https://example.com/api/users', {
  auth: {
    username: 'ユーザー名',
    password: 'パスワード'
  }
}).then((response) => {
  console.log(response.data); // APIからのレスポンスをコンソールに出力
}).catch((error) => {
  console.log(error.response.data); // エラーの場合、エラーレスポンスをコンソールに出力
});
```
基本認証を行うには、axiosなどのHTTPクライアントライブラリを使用します。リクエストを送信する際、`auth`オプションを使用して、ユーザー名とパスワードを含むオブジェクトを渡します。認証が必要な場合、サーバーからのレスポンスで認証エラーが返されることに注意してください。

## Deep Dive
基本認証では、ユーザー名とパスワードが平文として送信されるため、セキュリティ上のリスクがあります。そのため、HTTPSを使用して暗号化された接続を確立することが重要です。また、ユーザー名とパスワードを暗号化するため、Base64エンコーディングも使用されます。

## See Also
- [axios documentation](https://github.com/axios/axios)
- [HTTP Basic Authentication](https://developer.mozilla.org/ja/docs/Web/HTTP/Authentication#Basic_authentication_scheme)