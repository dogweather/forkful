---
date: 2024-01-20 17:37:29.559947-07:00
description: "How to: (\u65B9\u6CD5) ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:41.515136-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\u5909\u63DB\u3059\u308B"
weight: 28
---

## How to: (方法)
```python
from datetime import datetime

# 現在の日付と時刻を取得
current_datetime = datetime.now()

# 日付を文字列に変換
date_string = current_datetime.strftime("%Y-%m-%d %H:%M:%S")

# 出力
print(date_string)
```
出力例:
```
2023-03-15 17:45:31
```

## Deep Dive (深いダイブ)
日付データは扱いにくいです。Pythonの`datetime`モジュールは、日付と時刻を簡単に扱うために設計されています。`strftime`メソッドは、日付と時刻を文字列に変換する際、柔軟で強力な機能を提供します。書式コード（例: `%Y`は年を表す）を使って、出力の形式を指定できます。他の言語も同様の機能を持っていますが、Pythonでは見やすさと使いやすさが強調されています。

代替手段としては、`isoformat()`メソッドや外部ライブラリ（例: `arrow`や`pendulum`）を使用する方法もあります。`strftime`は標準ライブラリで広く使われているため、学ぶ価値が高いです。

Python 3.0以降、`datetime`は`str()`による簡易的な文字列変換もサポートしていますが、`strftime`を使用することでもっと細かい制御ができます。

## See Also (参照)
- Pythonの公式ドキュメント: https://docs.python.org/3/library/datetime.html#strftime-strptime-behavior
- strftime()関数と書式コードの完全なリスト: https://strftime.org/
- `arrow`: https://arrow.readthedocs.io
- `pendulum`: https://pendulum.eustace.io
