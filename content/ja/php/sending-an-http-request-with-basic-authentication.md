---
title:                "基本認証を使用して、httpリクエストを送信する"
html_title:           "PHP: 基本認証を使用して、httpリクエストを送信する"
simple_title:         "基本認証を使用して、httpリクエストを送信する"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## なぜ

基本認証を使用してHTTPリクエストを送信する理由は、セキュリティー上の理由です。基本認証は、ユーザーによる認証を行うための非常にシンプルで一般的な方法であり、URLの認証を行い、アクセス制御を行うことができます。

## やり方

基本認証を使用してHTTPリクエストを送信するには、まず次のようにPHPの`curl`モジュールを使用して新しいリクエストを作成する必要があります。

```PHP
$ch = curl_init();
```

次に、リクエストのアドレスと認証情報を指定する必要があります。

```PHP
curl_setopt($ch, CURLOPT_URL, 'https://example.com');
curl_setopt($ch, CURLOPT_USERPWD, 'username:password');
```

最後に、リクエストを実行し、レスポンスを取得します。

```PHP
curl_exec($ch);
curl_close($ch);
```

これで、基本認証を使用してHTTPリクエストを送信する準備が整いました。

## ディープダイブ

基本認証を使用してHTTPリクエストを送信する際、`curl`モジュールを使用することで、より詳細なオプションを指定することもできます。例えば、認証方法をBASIC以外のものに変更したり、ユーザー名とパスワードを外部から読み込んだりすることもできます。

## References

https://www.php.net/manual/en/book.curl.php

## はてなブログ

https://hatenablog.com/

## Guzzle

https://docs.guzzlephp.org/en/stable/