---
date: 2024-01-20 18:00:11.431329-07:00
description: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u9001\u308B\u3063\u3066\uFF1F\
  \u30C7\u30FC\u30BF\u4EA4\u63DB\u3060\u3002 \u30A6\u30A7\u30D6\u30B5\u30FC\u30D0\u3068\
  \u8A71\u3059\u3068\u304D\u306B\u4F7F\u3046\u3002\u306A\u305C\u304B\uFF1F\u60C5\u5831\
  \u3092\u5F97\u305F\u308A\u3001\u9001\u3063\u305F\u308A\u3059\u308B\u305F\u3081\u3055\
  \u3002"
isCJKLanguage: true
lastmod: 2024-02-19 22:05:00.767416
model: gpt-4-1106-preview
summary: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u9001\u308B\u3063\u3066\uFF1F\u30C7\
  \u30FC\u30BF\u4EA4\u63DB\u3060\u3002 \u30A6\u30A7\u30D6\u30B5\u30FC\u30D0\u3068\u8A71\
  \u3059\u3068\u304D\u306B\u4F7F\u3046\u3002\u306A\u305C\u304B\uFF1F\u60C5\u5831\u3092\
  \u5F97\u305F\u308A\u3001\u9001\u3063\u305F\u308A\u3059\u308B\u305F\u3081\u3055\u3002"
title: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306E\u9001\u4FE1"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)

HTTPリクエストを送るって？データ交換だ。
ウェブサーバと話すときに使う。なぜか？情報を得たり、送ったりするためさ。

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
