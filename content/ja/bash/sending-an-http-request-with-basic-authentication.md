---
title:                "Bash: ベーシック認証を使用してhttpリクエストを送信する"
simple_title:         "ベーシック認証を使用してhttpリクエストを送信する"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## なぜ
なぜ人々は基本認証を使用してHTTPリクエストを送信するかを説明する1-2文。

HTTPリクエストを送信する際に、セキュリティが重要な場合には、基本認証を使用することで、特定のユーザーのみがアクセスできるように制限することができます。

## 方法

```Bash
# cURLを使用して基本認証を行うHTTPリクエストを送信する
curl -u ユーザー名:パスワード URL
```

上記のコードでは、cURLを使用して基本認証を行うHTTPリクエストを送信しています。-uオプションを使用することで、ユーザー名とパスワードを指定することができます。

```Bash
# 201 Createdレスポンスを受け取る例
HTTP/1.1 201 Created
Content-Type: application/json

{
  "message": "Success",
  "data": {
    "id": "12345",
    "name": "John Doe"
  }
}
```

上記の例では、サーバーから201 Createdレスポンスを受け取っています。HTTPリクエストが成功したことを示すメッセージと、データの中にidと名前の情報が含まれています。

## 詳細について

基本認証を使用してHTTPリクエストを送信する際には、ユーザー名とパスワードをBase64エンコードして、リクエストのAuthorizationヘッダーに含める必要があります。これにより、サーバー側で正しい認証が行われることができます。

また、基本認証はセキュリティレベルが低いため、より高度な認証方法を使用することが推奨されています。セキュリティ上のリスクを考慮した上で、適切な認証方法を選択することが重要です。

## 併せて参照

- [cURLコマンドラインチートシート](https://catonmat.net/cookbooks/curl)
- [HTTPリクエストとヘッダーについて](https://developer.mozilla.org/ja/docs/Web/HTTP/Overview)
- [基本認証についての詳細](https://developer.mozilla.org/ja/docs/Web/HTTP/Authentication#%E5%9F%BA%E6%9C%AC%E8%AA%8D%E8%A8%BC)