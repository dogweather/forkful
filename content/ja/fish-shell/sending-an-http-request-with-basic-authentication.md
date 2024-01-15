---
title:                "基本認証でhttpリクエストを送信する"
html_title:           "Fish Shell: 基本認証でhttpリクエストを送信する"
simple_title:         "基本認証でhttpリクエストを送信する"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# なぜ

HTTPリクエストで基本認証を送信することのメリットは、セキュリティの向上です。これにより、アクセス制限がかかったウェブサイトやAPIに対して、認証情報を提供することができます。

# 方法

基本認証を使用してHTTPリクエストを送信するには、`curl`コマンドを使用します。以下のコードをターミナルで実行することで、認証情報を含んだリクエストを送信することができます。

```Fish Shell
curl -u <username>:<password> <URL>
```

`<username>`と`<password>`には、ウェブサイトやAPIに登録する際に指定した認証情報を入力し、`<URL>`にはリクエストを送信したい先のURLを入力します。

## 深く掘り下げる

基本認証を使用してHTTPリクエストを送信する際には、curlコマンドの`-u`オプションによって認証情報を指定する点に注意が必要です。認証情報が暗号化されていないため、セキュリティ性は低いと言えます。より安全な認証方式を使用したい場合は、OAuthやトークンベースの認証を検討することをおすすめします。

## 参考リンク

- [curl公式ドキュメント](https://curl.haxx.se/docs/)
- [基本認証についての詳細な説明(英語)](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#Basic_authentication_scheme)