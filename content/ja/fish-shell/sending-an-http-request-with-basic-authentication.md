---
title:                "基本認証を使用してHTTPリクエストを送信する"
html_title:           "Fish Shell: 基本認証を使用してHTTPリクエストを送信する"
simple_title:         "基本認証を使用してHTTPリクエストを送信する"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 何が & なぜ？
HTTPリクエストを基本認証で送信するとはどういうことか、そしてなぜプログラマーがそれを行うのかを説明します。

プログラマーがHTTPリクエストを基本認証で送信するのは、サイトやアプリケーションにアクセスする際に認証を必要とする場合があるからです。基本認証を使用すると、ユーザーのユーザー名とパスワードを使用して認証することができます。

## やり方：
```Fish Shell```のコードブロック内にコーディング例とサンプル出力を含めます。

```
# HTTPリクエストを基本認証で送信するコマンド
curl -u "ユーザー名:パスワード" URL
```

```
# サンプル出力
{
  "title": "こんにちは、世界！"
}
```

## 詳細説明：
HTTPリクエストを基本認証で送信するとは、実際にはどのようなことを意味するのでしょうか？基本認証は、Webサイトやアプリケーションにアクセスする際に使用される最も古典的な認証方法の一つです。ただし、それ以外の認証方法もあります。たとえば、OAuthやAPIキーなどがあります。

基本認証を使用することで、ユーザーのプライバシーが保護されると同時に、セキュアなアクセスが可能になります。しかし、ユーザーのパスワードがHTTPSで送信されない限り、ユーザーのプライバシーは守られません。

## 関連サイト：
- [curlコマンドのマニュアル](https://curl.haxx.se/docs/manual.html)
- [基本認証の詳細説明](https://developer.mozilla.org/ja/docs/Web/HTTP/Authentication)
- [HTTP認証の別のタイプ](https://developer.mozilla.org/ja/docs/Web/HTTP/Authentication#Other_authentication_methods)