---
title:                "基本認証を使用してhttpリクエストを送信する"
html_title:           "C#: 基本認証を使用してhttpリクエストを送信する"
simple_title:         "基本認証を使用してhttpリクエストを送信する"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 何となぜ？

HTTPリクエストに基本認証を使用するとは、単にユーザーネームとパスワードを提供してサーバーと通信を行うまっすぐな方法です。プログラマーがこれを行う理由は、情報をセキュアに送受信するため、または特定のWebサービスに認証情報を提供するためといったものです。

## どうするの？

```PowerShell
$URL = 'http://example.com/'
$Username = 'user'
$Password = 'pass123'
$Base64AuthInfo = [Convert]::ToBase64String([Text.Encoding]::ASCII.GetBytes(("{0}:{1}" -f $Username,$Password)))
$result = Invoke-RestMethod -Uri $URL -Headers @{Authorization=("Basic {0}" -f $Base64AuthInfo)}
```
```PowerShell
echo $result
```

結果を得るために`echo $result`を使うことができます。

## ディープダイブ 

基本認証でHTTPリクエストを送信するためのこの形式は、古くから存在していて新しいテクノロジーに取って代わられてない安定した方法です。しかしながら、OAuthなど他のより現代的で安全な認証手段が開発され、多くのサービスはしだいにそちらに移行しています。

PowerShellにおける基本認証のHTTPリクエストの実装は、ヘッダー情報の手動エンコードを通じて行われます。それは認証情報（ユーザーネームとパスワード）をBase64形式にエンコードし、 `Authorization`ヘッダーにその情報を埋め込むというプロセスです。

## 参考情報

- PowerShellのInvoke-RestMethodについての詳細は[こちら](https://docs.microsoft.com/ja-jp/powershell/module/microsoft.powershell.utility/invoke-restmethod?view=powershell-7.1)から確認できます。
- 基本認証について更に深く知るための[リンク](https://developer.mozilla.org/ja/docs/Web/HTTP/Authentication)です。
- より現代的な認証方法であるOAuthについての[詳細](https://oauth.net/)です。