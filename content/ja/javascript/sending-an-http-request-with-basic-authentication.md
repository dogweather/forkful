---
title:                "基本認証を使用してhttpリクエストを送信する方法"
html_title:           "Javascript: 基本認証を使用してhttpリクエストを送信する方法"
simple_title:         "基本認証を使用してhttpリクエストを送信する方法"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 何となく分かる？
HTTPリクエストを基本認証で送信するとは、プログラマーが特定のリソースにアクセスするために認証情報を送信することです。基本認証を使う理由は、セキュリティの上で信頼性が高く、簡単に実装できるからです。

## 実際にやってみよう

```Javascript
fetch('https://example.com/api/resource', {
  method: 'GET',
  headers: {
    Authorization: 'Basic ' + btoa(username + ':' + password)
  }
})
  .then(response => response.json())
  .then(data => console.log(data));
```

基本認証では、認証ヘッダーにBase64エンコードされたユーザー名とパスワードを含めるだけで、リクエストを送信することができます。上記の例では、fetchメソッドを使用してAPIからリソースを取得し、認証情報を含めたリクエストを送信しています。

## 深堀する

### 歴史的背景
基本認証は、HTTPの最初の認証メカニズムの1つとして、1999年にRFC 2617として発表されました。その後もうまく運用されており、今でも多くのWebサーバーやAPIで利用されています。

### 代替手段
基本認証は、セキュリティの観点からはあまり安全ではありません。パスワードの暗号化やトークンベースの認証など、より安全な代替手段があります。しかし、基本認証は実装が簡単でコストがかからないため、一部のシステムではまだ使用されています。

### 実装の詳細
基本認証を実装するには、認証情報を含むAuthorizationヘッダーをリクエストに追加する必要があります。また、認証情報はBase64でエンコードする必要があります。サーバー側では、リクエストに含まれる認証情報をデコードし、ユーザー名とパスワードをチェックして認証処理を行います。

## 他にも見てみよう
- [HTTP基本認証 - MDN Web Docs](https://developer.mozilla.org/ja/docs/Web/HTTP/Authentication#basic_authentication_scheme)
- [Basic認証とディジェスト認証の違いについて - Qiita](https://qiita.com/hsatac/items/25e2a7f33430a2e245b2)
- [Fetch APIを試してみよう - Qiita](https://qiita.com/tonkotsuboy_com/items/0a8c72e8cefd1e0881c7)