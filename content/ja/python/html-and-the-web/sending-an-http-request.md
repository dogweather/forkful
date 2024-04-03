---
date: 2024-01-20 18:00:11.431329-07:00
description: "How to: (\u3084\u308A\u65B9) ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:41.495934-06:00'
model: gpt-4-1106-preview
summary: .
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
