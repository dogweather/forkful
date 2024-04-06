---
date: 2024-01-20 17:32:01.174863-07:00
description: "How to: (\u3084\u308A\u65B9) Python\u3067\u672A\u6765\u3084\u904E\u53BB\
  \u306E\u65E5\u4ED8\u3092\u8A08\u7B97\u3059\u308B\u306B\u306F\u3001`datetime`\u30E2\
  \u30B8\u30E5\u30FC\u30EB\u3092\u4F7F\u3044\u307E\u3059\u3002\u4F8B\u3092\u898B\u3066\
  \u307F\u307E\u3057\u3087\u3046\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:42.469514-06:00'
model: gpt-4-1106-preview
summary: "(\u3084\u308A\u65B9) Python\u3067\u672A\u6765\u3084\u904E\u53BB\u306E\u65E5\
  \u4ED8\u3092\u8A08\u7B97\u3059\u308B\u306B\u306F\u3001`datetime`\u30E2\u30B8\u30E5\
  \u30FC\u30EB\u3092\u4F7F\u3044\u307E\u3059\u3002\u4F8B\u3092\u898B\u3066\u307F\u307E\
  \u3057\u3087\u3046\u3002"
title: "\u5C06\u6765\u307E\u305F\u306F\u904E\u53BB\u306E\u65E5\u4ED8\u3092\u8A08\u7B97\
  \u3059\u308B"
weight: 26
---

## How to: (やり方)
Pythonで未来や過去の日付を計算するには、`datetime`モジュールを使います。例を見てみましょう。

```Python
from datetime import datetime, timedelta

# 今日の日付を取得
today = datetime.now()

# 10日後の日付を計算
future_date = today + timedelta(days=10)
print(f"10日後: {future_date.strftime('%Y-%m-%d')}")

# 5日前の日付を計算
past_date = today - timedelta(days=5)
print(f"5日前: {past_date.strftime('%Y-%m-%d')}")
```

出力例:
```
10日後: 2023-04-15
5日前: 2023-04-01
```

## Deep Dive (深掘り)
日付の計算は長い歴史を持ち、アナログからデジタルへの移行が進むにつれ、その方法も進化しました。Pythonの`datetime`モジュールは、日付や時間の計算における豊富な機能を提供します。`timedelta`オブジェクトは差分を扱い、日付を足したり引いたりするのに使われます。`datetime`以外にも、`dateutil`ライブラリがありますが、標準ライブラリのみで十分な場合が多いです。実装の詳細では、うるう年やタイムゾーンの考慮が必要になることがあります。

## See Also (関連情報)
- Pythonの公式`datetime`ドキュメント: https://docs.python.org/3/library/datetime.html
- `dateutil`ライブラリのドキュメント: https://dateutil.readthedocs.io/en/stable/
- ブログやチュートリアルも役立つ情報がたくさんあります。例えば、Real Pythonの記事: https://realpython.com/python-datetime/
