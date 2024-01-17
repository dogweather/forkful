---
title:                "基本認証付きでのhttpリクエストの送信"
html_title:           "PowerShell: 基本認証付きでのhttpリクエストの送信"
simple_title:         "基本認証付きでのhttpリクエストの送信"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 何をするためのものか？

ベーシック認証でHTTPリクエストを送るとは、インターネット上でコンピューター間のデータのやりとりを実現する方法の一つです。プログラマーがこれを行う理由は、セキュリティ上の理由にあります。ベーシック認証を使用することで、リクエストを送信する際にユーザー名とパスワードを使用してユーザーを認証することができます。

## 方法：

```PowerShell
# Invoke-WebRequestを使用してHTTPリクエストを送信する例
# オプションでユーザー名とパスワードを指定し、Basic認証を追加することができます。
# 「-UseBasicParsing」オプションを使用することによって、インターネットエクスプローラーのアドオンを使用するよりも高速でリクエストを送ることができます。

Invoke-WebRequest -Uri "https://example.com/" -Method GET -Credential (Get-Credential) -UseBasicParsing
```

```PowerShell
# Invoke-RestMethodを使用してHTTPリクエストを送信する例
# オプションでユーザー名とパスワードを指定し、Basic認証を追加することができます。
# 「-UseBasicParsing」オプションを使用することによって、インターネットエクスプローラーのアドオンを使用するよりも高速でリクエストを送ることができます。

Invoke-RestMethod -Uri "https://example.com/" -Method GET -Credential (Get-Credential) -UseBasicParsing
```

## 深堀り

ベーシック認証は、1990年代に開発された初期のインターネットプロトコルの一つであり、今でも多くのウェブサービスで使用されています。しかし、プライバシーの観点やセキュリティ強化の要求により、ベーシック認証の代替方法が現在ではより多く使用されています。これらの代替方法には、「ダイジェスト認証」や「OAuth」と呼ばれるものがあります。実際にコードを書く際には、ベーシック認証の負荷と比較して、より安全な方法でHTTPリクエストを送ることをお勧めします。

## 関連リンク

- [記事タイトルの理由を理解する](https://www.example.com/why-send-http-request-basic-auth)
- [インターネットプロトコルの歴史を学ぶ](https://www.example.com/internet-protocol-history)
- [ベーシック認証を行うための他のプログラミング言語の例](https://www.example.com/other-programming-language-basic-auth-examples)