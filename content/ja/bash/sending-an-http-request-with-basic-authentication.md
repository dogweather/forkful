---
title:                "基本認証を使用してhttpリクエストを送信する"
html_title:           "C#: 基本認証を使用してhttpリクエストを送信する"
simple_title:         "基本認証を使用してhttpリクエストを送信する"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 何となぜ？

HTTPリクエストに基本認証を付けて送信するとは、特定のWebページへのアクセスが許可されたユーザーのみに制限する方法です。プログラマはこれを使用して、ウェブコンテンツを不正アクセスから保護します。

## やり方：

以下のコードサンプルでは、curlコマンドを使ってHTTPリクエストに基本認証を付けて送信しています。

```Bash 
user='YOUR_USERNAME'
pass='YOUR_PASSWORD'

curl -u $user:$pass http://yourwebsite.com
```
正常な出力は以下のようになります：
```Bash
<!DOCTYPE html> .....
```

## ディープダイブ：

基本認証というのは、HTTPプロトコルが初めて導入されたときから存在するものです。伝統的にはWebサーバがクライアントに対して要求する簡単な認証方法として使われてきました。

しかし、パスワードが平文で送信されるため、SSL/TLSなどの暗号化技術と一緒に使用しない限り、セキュリティは保証されません。そこで、OAuthなどのより安全な認証方法も一般的に利用されます。

具体的な実装については、各HTTPリクエストのヘッダーに `Authorization: Basic base64(user:pass)`を追加しています。ここでbase64はユーザー名とパスワードをエンコードするメソッドです。

## 参照：

1. [教育用マテリアル （Bash）](https://www.gnu.org/software/bash/manual/bash.html)
2. [Basic認証の詳細](https://developer.mozilla.org/ja/docs/Web/HTTP/Authentication)
3. [代替の認証方法(OAuth)](https://ja.wikipedia.org/wiki/OAuth)