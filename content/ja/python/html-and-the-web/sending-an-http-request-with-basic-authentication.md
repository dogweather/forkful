---
date: 2024-01-20 18:02:49.440428-07:00
description: "How to (\u3084\u308A\u65B9): Python\u306E`requests`\u30E9\u30A4\u30D6\
  \u30E9\u30EA\u3092\u4F7F\u7528\u3057\u3066\u3001Basic\u8A8D\u8A3C\u4ED8\u304D\u3067\
  HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u9001\u4FE1\u3059\u308B\u65B9\u6CD5\u3092\
  \u793A\u3057\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:42.452758-06:00'
model: gpt-4-1106-preview
summary: "Python\u306E`requests`\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u4F7F\u7528\u3057\
  \u3066\u3001Basic\u8A8D\u8A3C\u4ED8\u304D\u3067HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\
  \u3092\u9001\u4FE1\u3059\u308B\u65B9\u6CD5\u3092\u793A\u3057\u307E\u3059\u3002"
title: "\u57FA\u672C\u8A8D\u8A3C\u3092\u4F7F\u7528\u3057\u305FHTTP\u30EA\u30AF\u30A8\
  \u30B9\u30C8\u306E\u9001\u4FE1"
weight: 45
---

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
