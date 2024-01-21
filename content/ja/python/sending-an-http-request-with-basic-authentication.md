---
title:                "基本認証を使用したHTTPリクエストの送信"
date:                  2024-01-20T18:02:49.440428-07:00
model:                 gpt-4-1106-preview
simple_title:         "基本認証を使用したHTTPリクエストの送信"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/sending-an-http-request-with-basic-authentication.md"
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