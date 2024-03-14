---
date: 2024-01-20 18:02:49.440428-07:00
description: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306B\u57FA\u672C\u8A8D\u8A3C\u3092\
  \u4ED8\u52A0\u3059\u308B\u3053\u3068\u306F\u3001\u30E6\u30FC\u30B6\u30FC\u540D\u3068\
  \u30D1\u30B9\u30EF\u30FC\u30C9\u3067\u4FDD\u8B77\u3055\u308C\u305F\u30EA\u30BD\u30FC\
  \u30B9\u306B\u30A2\u30AF\u30BB\u30B9\u3059\u308B\u65B9\u6CD5\u3067\u3059\u3002\u30D7\
  \u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u30BB\u30AD\u30E5\u30A2\u306AAPI\u30A8\u30F3\
  \u30C9\u30DD\u30A4\u30F3\u30C8\u3078\u306E\u30A2\u30AF\u30BB\u30B9\u3084\u3001\u8A8D\
  \u8A3C\u304C\u5FC5\u8981\u306A\u30EA\u30BD\u30FC\u30B9\u306E\u53D6\u5F97\u306B\u3053\
  \u308C\u3092\u884C\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:41.499290-06:00'
model: gpt-4-1106-preview
summary: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306B\u57FA\u672C\u8A8D\u8A3C\u3092\u4ED8\
  \u52A0\u3059\u308B\u3053\u3068\u306F\u3001\u30E6\u30FC\u30B6\u30FC\u540D\u3068\u30D1\
  \u30B9\u30EF\u30FC\u30C9\u3067\u4FDD\u8B77\u3055\u308C\u305F\u30EA\u30BD\u30FC\u30B9\
  \u306B\u30A2\u30AF\u30BB\u30B9\u3059\u308B\u65B9\u6CD5\u3067\u3059\u3002\u30D7\u30ED\
  \u30B0\u30E9\u30DE\u30FC\u306F\u30BB\u30AD\u30E5\u30A2\u306AAPI\u30A8\u30F3\u30C9\
  \u30DD\u30A4\u30F3\u30C8\u3078\u306E\u30A2\u30AF\u30BB\u30B9\u3084\u3001\u8A8D\u8A3C\
  \u304C\u5FC5\u8981\u306A\u30EA\u30BD\u30FC\u30B9\u306E\u53D6\u5F97\u306B\u3053\u308C\
  \u3092\u884C\u3044\u307E\u3059\u3002"
title: "\u57FA\u672C\u8A8D\u8A3C\u3092\u4F7F\u7528\u3057\u305FHTTP\u30EA\u30AF\u30A8\
  \u30B9\u30C8\u306E\u9001\u4FE1"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
HTTPリクエストに基本認証を付加することは、ユーザー名とパスワードで保護されたリソースにアクセスする方法です。プログラマーはセキュアなAPIエンドポイントへのアクセスや、認証が必要なリソースの取得にこれを行います。

## How to (やり方):
Pythonの`requests`ライブラリを使用して、Basic認証付きでHTTPリクエストを送信する方法を示します。

```python
import requests
from requests.auth import HTTPBasicAuth

# 認証情報の設定
username = 'your_username'
password = 'your_password'

# 対象のURL
url = 'https://api.example.com/data'

# Basic認証でGETリクエストを送信
response = requests.get(url, auth=HTTPBasicAuth(username, password))

# レスポンス内容の表示
print(response.status_code)
print(response.text)
```

このコードは、指定された`url`に対して`username`と`password`でBasic認証を用いてGETリクエストを送信し、応答を表示します。

## Deep Dive (深掘り):
### 歴史的背景
Basic認証はHTTPプロトコルにおける最も古い認証方式の一つです。1996年にRFC 1945で初めて定義されましたが、平文のユーザー名とパスワードをBase64でエンコードするだけであるため、現代のセキュリティ基準には適していません。

### 代替手段
よりセキュアな認証手段として、OAuthやJWT (JSON Web Tokens) があります。これらはトークンベースの認証を提供し、Basic認証よりもセキュアで柔軟です。

### 実装の詳細
`requests`ライブラリは内部でBase64エンコーディングを自動的に処理します。HTTPS経由での使用が推奨されるのは、Basic認証情報がエンコードされても暗号化されないためです。攻撃者によって簡単にデコードされる危険があるからです。

## See Also (参照):
- RFC 7617, The 'Basic' HTTP Authentication Scheme: https://tools.ietf.org/html/rfc7617
- Requestsライブラリのドキュメント: https://requests.readthedocs.io/en/master/
- Python公式ドキュメント: https://docs.python.org/3/library/requests.html#requests.Request
- OAuth: https://oauth.net/
- JWT (JSON Web Tokens): https://jwt.io/
