---
title:                "HTTPリクエストの送信"
date:                  2024-01-20T18:00:11.431329-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTPリクエストの送信"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/sending-an-http-request.md"
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
