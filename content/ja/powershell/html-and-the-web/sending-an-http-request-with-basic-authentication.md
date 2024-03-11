---
date: 2024-01-20 18:02:25.850235-07:00
description: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306B\u57FA\u672C\u8A8D\u8A3C\u3092\
  \u52A0\u3048\u308B\u3063\u3066\uFF1FWeb\u30B5\u30FC\u30D3\u30B9\u306B\u30E6\u30FC\
  \u30B6\u540D\u3068\u30D1\u30B9\u30EF\u30FC\u30C9\u3067\u30ED\u30B0\u30A4\u30F3\u3055\
  \u305B\u308B\u3093\u3067\u3059\u3002\u3053\u308C\u3092\u3059\u308B\u7406\u7531\uFF1F\
  \u5B89\u5168\u306BAPI\u3068\u304B\u30C7\u30FC\u30BF\u306B\u30A2\u30AF\u30BB\u30B9\
  \u3057\u305F\u3044\u304B\u3089\u3067\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-11T00:14:15.988666-06:00'
model: gpt-4-1106-preview
summary: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306B\u57FA\u672C\u8A8D\u8A3C\u3092\u52A0\
  \u3048\u308B\u3063\u3066\uFF1FWeb\u30B5\u30FC\u30D3\u30B9\u306B\u30E6\u30FC\u30B6\
  \u540D\u3068\u30D1\u30B9\u30EF\u30FC\u30C9\u3067\u30ED\u30B0\u30A4\u30F3\u3055\u305B\
  \u308B\u3093\u3067\u3059\u3002\u3053\u308C\u3092\u3059\u308B\u7406\u7531\uFF1F\u5B89\
  \u5168\u306BAPI\u3068\u304B\u30C7\u30FC\u30BF\u306B\u30A2\u30AF\u30BB\u30B9\u3057\
  \u305F\u3044\u304B\u3089\u3067\u3059\u3002"
title: "\u57FA\u672C\u8A8D\u8A3C\u3092\u4F7F\u7528\u3057\u305FHTTP\u30EA\u30AF\u30A8\
  \u30B9\u30C8\u306E\u9001\u4FE1"
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
