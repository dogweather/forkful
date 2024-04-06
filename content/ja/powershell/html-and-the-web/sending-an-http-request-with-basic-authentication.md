---
date: 2024-01-20 18:02:25.850235-07:00
description: "How to: (\u3084\u308A\u65B9) \u5B9F\u884C\u7D50\u679C\u306E\u30B5\u30F3\
  \u30D7\u30EB."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.258714-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u57FA\u672C\u8A8D\u8A3C\u3092\u4F7F\u7528\u3057\u305FHTTP\u30EA\u30AF\u30A8\
  \u30B9\u30C8\u306E\u9001\u4FE1"
weight: 45
---

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
