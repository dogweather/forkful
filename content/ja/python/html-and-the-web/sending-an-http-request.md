---
date: 2024-01-20 18:00:11.431329-07:00
description: "How to: (\u3084\u308A\u65B9) \u6B74\u53F2\u7684\u80CC\u666F\uFF1AHTTP\u306F\
  \u30A6\u30A7\u30D6\u306E\u4E2D\u6838\u30021991\u5E74\u306B\u767B\u5834\u3002 \u4EE3\
  \u66FF\u624B\u6BB5\uFF1A`http.client` \u3084 `urllib` \u3067\u3082\u3067\u304D\u308B\
  \u304C\u3001`requests` \u306F\u7C21\u5358\u3002 \u5B9F\u88C5\u306E\u8A73\u7D30\uFF1A\
  `requests` \u306F\u5185\u90E8\u3067 `urllib3` \u3092\u4F7F\u7528\u3002\u5B89\u5168\
  \u3067\u4F7F\u3044\u3084\u3059\u3044API\u3092\u63D0\u4F9B\u3059\u308B\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T22:37:49.832030-06:00'
model: gpt-4-1106-preview
summary: "(\u3084\u308A\u65B9) \u6B74\u53F2\u7684\u80CC\u666F\uFF1AHTTP\u306F\u30A6\
  \u30A7\u30D6\u306E\u4E2D\u6838\u30021991\u5E74\u306B\u767B\u5834\u3002"
title: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306E\u9001\u4FE1"
weight: 44
---

## How to: (やり方)
```Python
# requests ライブラリを使おう
import requests

# とあるURLにGETリクエストを送る
response = requests.get('https://api.github.com')

# ステータスコードをチェック
print(response.status_code)  # 200が出れば成功だ

# レスポンスの本文を見る
print(response.text)
```

出力例:

```
200
{"current_user_url":"https://api.github.com/user","current_user_authorizations_html_url":"https://github.com/settings/connections/applications{/client_id}", ...}
```

## Deep Dive (掘り下げ)
歴史的背景：HTTPはウェブの中核。1991年に登場。
代替手段：`http.client` や `urllib` でもできるが、`requests` は簡単。
実装の詳細：`requests` は内部で `urllib3` を使用。安全で使いやすいAPIを提供する。

## See Also (関連情報)
- Requests公式ドキュメント: https://requests.readthedocs.io/
- Python HTTPリクエストの更なる情報: https://realpython.com/python-requests/
- HTTPステータスコードの一覧: https://developer.mozilla.org/ja/docs/Web/HTTP/Status
