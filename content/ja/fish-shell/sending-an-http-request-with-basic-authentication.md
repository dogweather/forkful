---
title:                "基本認証を使用してhttpリクエストを送信する"
html_title:           "C#: 基本認証を使用してhttpリクエストを送信する"
simple_title:         "基本認証を使用してhttpリクエストを送信する"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 何となぜ？

HTTPリクエストを認証付きで送信するとは、コンピュータがウェブサーバーに対して情報を要求するための手段です。プログラマはこれを使用して、特定のユーザーがアクセスできるリソースにアクセスします。

## 実施方法:

```Fish Shell

# 必要なパッケージをインストールします。
fisher install jethrokuan/fzf

# ベーシック認証でHTTPリクエストを送信します。
set url "http://your-url.com"
set username "your-username"
set password "your-password"
curl -u $username:$password $url

```

あなたは以上の構文を使って、認証情報を含むHTTPリクエストを送信することができます。

## ディープダイブ：

初期のインターネットでは、HTTPリクエストはデータ交換の基盤でした。基本認証は、この通信をセキュアに保つための早期の手段であり、一部の情報を伝えるために依然として使用されています。

しかし現在では、OAuthなどのより高度な認証メカニズムが主流となっています。それでも基本認証は簡単であるため、プライベートネットワークや開発環境など、対外的に公開しない場所ではまだ活用されています。

基本的に、基本認証を使用してHTTPリクエストを送信する際の短所は、認証情報が平文（エンコードされているものの暗号化されていない）で送信されるため、ネットワーク上でそれを取り扱うすべての人々によって見ることができることです。

## 関連リンク：

HTTPリクエストの詳細:
http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html

基本認証の詳細:
https://ja.wikipedia.org/wiki/Basic認証

より高度な認証メカニズム (OAuth):
https://ja.wikipedia.org/wiki/OAuth