---
title:                "基本認証を使用したHTTPリクエストの送信"
aliases:
- /ja/powershell/sending-an-http-request-with-basic-authentication/
date:                  2024-01-20T18:02:25.850235-07:00
model:                 gpt-4-1106-preview
simple_title:         "基本認証を使用したHTTPリクエストの送信"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
HTTPリクエストに基本認証を加えるって？Webサービスにユーザ名とパスワードでログインさせるんです。これをする理由？安全にAPIとかデータにアクセスしたいからです。

## How to: (やり方)
```PowerShell
# ユーザ名とパスワードを設定
$user = 'your_username'
$pass = 'your_password'

# Basic認証のためのエンコード済みの資格情報を作成
$base64AuthInfo = [Convert]::ToBase64String([Text.Encoding]::ASCII.GetBytes(("{0}:{1}" -f $user, $pass)))

# HTTPヘッダに認証情報を追加
$headers = @{
    Authorization=("Basic {0}" -f $base64AuthInfo)
}

# HTTP GETリクエストを実行し、結果を表示
$response = Invoke-RestMethod -Uri 'https://api.example.com/data' -Method Get -Headers $headers
$response
```

実行結果のサンプル:
```PowerShell
# 例: JSON形式のデータが返される
{
    "data": "ここには取得したデータが表示されます"
}
```

## Deep Dive (詳細解説)
歴史的背景: 基本認証はHTTP/1.0からある古い方法。古くても、内部ツールなどセキュリティ要件が緩い場面で使われる。

代替手段: もっと安全？OAuthやトークン認証が今は主流。でも、シンプルが一番な時もある。

実装詳細: Basic認証では`Authorization`ヘッダに`Basic`の後にBase64でエンコードされた`username:password`を追加します。ただし、HTTPSを使わないと、情報が漏れる可能性があるので要注意。

## See Also (関連情報)
- [PowerShell の Invoke-RestMethod](https://docs.microsoft.com/ja-jp/powershell/module/microsoft.powershell.utility/invoke-restmethod?view=powershell-7.1) - HTTPリクエストを簡単に送るコマンド。
- [RFC 7617 - The 'Basic' HTTP Authentication Scheme](https://tools.ietf.org/html/rfc7617) - 基本認証の仕様を定めた文書。
- [OAuth 2.0](https://oauth.net/2/) - よりセキュアな認証方法に関する情報。
