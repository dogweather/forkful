---
title:                "基本認証を使用したHTTPリクエストの送信"
html_title:           "Bash: 基本認証を使用したHTTPリクエストの送信"
simple_title:         "基本認証を使用したHTTPリクエストの送信"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 何をして、なぜ？
基本認証を使用してHTTPリクエストを送信することは、ユーザーがWebサービスやアプリケーションにアクセスする際に、ユーザー名とパスワードの組み合わせを使って認証する方法です。プログラマーは、ユーザーのアカウントを保護し、セキュリティを強化するためにこの方法を使用します。

## 方法：
```Bash
curl --user user:password URL
```

このコマンドは、指定したURLに対してユーザー名とパスワードの情報を付加して、HTTPリクエストを送信します。もし認証が成功すると、サーバーからレスポンスが返されます。

```Bash
> GET / HTTP/1.1
> Host: example.com
> Authorization: Basic dXNlcjpwYXNzd29yZA==
```

これは、基本認証を使用してexample.comのルートにアクセスするコマンドです。ユーザー名は「user」、パスワードは「password」で、認証情報はBase64エンコードされます。

## 詳細：
基本認証は、HTTPプロトコルの一部として1990年代に開発されました。現在でも、多くのWebサービスやAPIで使用されていますが、セキュリティ上のリスクがあるため、より安全な認証方法も開発されています。例えば、OAuthやJWTなどがあります。

また、基本認証を使用する際には、パスワードが平文で送信されるため、HTTPSを使用することが推奨されます。そのため、高度なセキュリティを求められる場合には、HTTPSを使用した認証方法を検討することが重要です。

## 関連情報：
- [cURL Documentation](https://curl.haxx.se/docs/manpage.html)
- [HTTP Basics: Authentication](https://www.youtube.com/watch?v=twcpJ_mtm5E)
- [OAuth 2.0](https://oauth.net/2/)