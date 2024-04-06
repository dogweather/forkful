---
date: 2024-01-20 18:02:25.850235-07:00
description: "How to: (\u3084\u308A\u65B9) \u6B74\u53F2\u7684\u80CC\u666F: \u57FA\u672C\
  \u8A8D\u8A3C\u306FHTTP/1.0\u304B\u3089\u3042\u308B\u53E4\u3044\u65B9\u6CD5\u3002\
  \u53E4\u304F\u3066\u3082\u3001\u5185\u90E8\u30C4\u30FC\u30EB\u306A\u3069\u30BB\u30AD\
  \u30E5\u30EA\u30C6\u30A3\u8981\u4EF6\u304C\u7DE9\u3044\u5834\u9762\u3067\u4F7F\u308F\
  \u308C\u308B\u3002 \u4EE3\u66FF\u624B\u6BB5: \u3082\u3063\u3068\u5B89\u5168\uFF1F\
  OAuth\u3084\u30C8\u30FC\u30AF\u30F3\u8A8D\u8A3C\u304C\u4ECA\u306F\u4E3B\u6D41\u3002\
  \u3067\u3082\u3001\u30B7\u30F3\u30D7\u30EB\u304C\u4E00\u756A\u306A\u6642\u3082\u3042\
  \u308B\u3002 \u5B9F\u88C5\u8A73\u7D30:\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:50:56.327309-06:00'
model: gpt-4-1106-preview
summary: "(\u3084\u308A\u65B9) \u6B74\u53F2\u7684\u80CC\u666F: \u57FA\u672C\u8A8D\u8A3C\
  \u306FHTTP/1.0\u304B\u3089\u3042\u308B\u53E4\u3044\u65B9\u6CD5\u3002\u53E4\u304F\
  \u3066\u3082\u3001\u5185\u90E8\u30C4\u30FC\u30EB\u306A\u3069\u30BB\u30AD\u30E5\u30EA\
  \u30C6\u30A3\u8981\u4EF6\u304C\u7DE9\u3044\u5834\u9762\u3067\u4F7F\u308F\u308C\u308B\
  \u3002 \u4EE3\u66FF\u624B\u6BB5: \u3082\u3063\u3068\u5B89\u5168\uFF1FOAuth\u3084\
  \u30C8\u30FC\u30AF\u30F3\u8A8D\u8A3C\u304C\u4ECA\u306F\u4E3B\u6D41\u3002\u3067\u3082\
  \u3001\u30B7\u30F3\u30D7\u30EB\u304C\u4E00\u756A\u306A\u6642\u3082\u3042\u308B\u3002\
  \ \u5B9F\u88C5\u8A73\u7D30: Basic\u8A8D\u8A3C\u3067\u306F`Authorization`\u30D8\u30C3\
  \u30C0\u306B`Basic`\u306E\u5F8C\u306BBase64\u3067\u30A8\u30F3\u30B3\u30FC\u30C9\u3055\
  \u308C\u305F`username:password`\u3092\u8FFD\u52A0\u3057\u307E\u3059\u3002\u305F\u3060\
  \u3057\u3001HTTPS\u3092\u4F7F\u308F\u306A\u3044\u3068\u3001\u60C5\u5831\u304C\u6F0F\
  \u308C\u308B\u53EF\u80FD\u6027\u304C\u3042\u308B\u306E\u3067\u8981\u6CE8\u610F\u3002"
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
