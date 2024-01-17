---
title:                "基本認証を使用してhttpリクエストを送信する"
html_title:           "TypeScript: 基本認証を使用してhttpリクエストを送信する"
simple_title:         "基本認証を使用してhttpリクエストを送信する"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 何と、なぜ？

HTTP要求を基本認証付きで送信することは何かというと、ユーザー名とパスワードなどのクレデンシャル情報をサーバーに安全に送信することです。なぜプログラマーがこれをするのかというと、アプリケーションやサービスへのアクセスを制限するために使用される、セキュリティ上の重要な手段だからです。

## 方法：

```TypeScript
// HTTPライブラリをインポート
import axios from 'axios';

// 基本認証を使用してHTTPリクエストを送信する例
axios.get('https://example.com/api', {
  auth: {
    username: 'ユーザー名',
    password: 'パスワード'
  }
})
  .then(response => {
    console.log(response.data);
  })
  .catch(err => {
    console.log(err);
  });
```

## 深堀り：

基本認証は、HTTPプロトコルの認証スキームの1つです。ユーザー名とパスワードは、Base 64でエンコードされ、HTTPリクエストヘッダー内のAuthorizationフィールドに含まれます。これは、ユーザーがアクセスを要求したリソースに関連する権限があるかどうかをチェックするために、サーバー側でデコードされます。

基本認証には、メールやFTPなど、多くのプロトコルで使用されてきましたが、現在は安全性の面で欠点があるとされ、代わりによりセキュアな認証方式が推奨されています。

基本認証を実装する方法は、HTTPライブラリを使用した例のように非常に簡単です。ただし、エンコードやデコードの処理など、細かな手順も理解しておく必要があります。

## 関連リンク：

- [HTTP Basic Authentication](https://en.wikipedia.org/wiki/Basic_access_authentication)
- [Examples of Using Basic Authentication in Different Programming Languages](http://docs.moodle.org/dev/Basic_authentication_examples)