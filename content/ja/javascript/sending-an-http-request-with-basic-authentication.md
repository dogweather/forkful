---
title:                "Javascript: 基本認証付きでhttpリクエストを送信する"
simple_title:         "基本認証付きでhttpリクエストを送信する"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## なぜHTTPリクエストを基本認証で送るのか

プログラムを書く際、時々ウェブサイトから情報を取得する必要があります。そのために、HTTPリクエストを送ることがあります。しかし、時には情報にアクセスするために認証が必要な場合があります。そのような場合、基本認証を使用してHTTPリクエストを送ることが必要になります。

## 方法

基本認証を使用するためには、次のコードを使用します。

```Javascript
var xhr = new XMLHttpRequest(); // XMLHTTPリクエストオブジェクトを作成する
xhr.open('GET', 'https://example.com/api', true); // リクエストを開く
xhr.setRequestHeader('Authorization', 'Basic ' + btoa('username:password')); // 認証情報をセットする
xhr.send(); // リクエストを送信する
```

上記のコードでは、XMLHttpRequestオブジェクトを作成し、リクエストURLを指定します。その後、setRequestHeaderメソッドを使用して、基本認証のヘッダー情報をセットします。最後に、sendメソッドを使用してリクエストを送信します。このようにすることで、認証情報を提供することができます。

基本認証を使用してリクエストを送ると、サーバー側では以下のようなヘッダーを返します。

```Javascript
{
  'Authorization': 'Basic dXNlcm5hbWU6cGFzc3dvcmQ=' // 送信した認証情報に基づき生成されたBase64エンコードされた文字列
}
```

これにより、サーバー側で認証情報を検証し、リクエストに応答することができます。

## ディープダイブ

基本認証を使用する主な理由は、セキュリティです。この方法を使用することで、ユーザー名とパスワードを含む認証情報を暗号化し、安全にリクエストを送信することができます。また、基本認証にはユーザー名とパスワードの組み合わせが必要で、よりセキュアな認証方法よりも簡単に実装することができます。

基本認証は、様々なウェブアプリケーションで使用されており、よく使われる認証の一つです。しかし、パスワードを平文で送信するため、HTTPSを使用してリクエストを送信することが推奨されます。

## See Also

- [XMLHttpRequestのドキュメント](https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest)
- [HTTPリクエストの基本認証についての詳細なドキュメント](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#Basic_authentication_scheme)