---
date: 2024-01-20 17:37:29.559947-07:00
description: "\u30C7\u30FC\u30C8\u3092\u6587\u5B57\u5217\u306B\u5909\u63DB\u3059\u308B\
  \u3068\u306F\u3001\u30D7\u30ED\u30B0\u30E9\u30E0\u5185\u306E\u65E5\u4ED8\u30C7\u30FC\
  \u30BF\u3092\u4EBA\u304C\u8AAD\u3081\u308B\u30C6\u30AD\u30B9\u30C8\u5F62\u5F0F\u306B\
  \u3059\u308B\u3053\u3068\u3067\u3059\u3002\u30ED\u30B0\u51FA\u529B\u3001\u30E6\u30FC\
  \u30B6\u30FC\u30A4\u30F3\u30BF\u30FC\u30D5\u30A7\u30A4\u30B9\u3001\u30C7\u30FC\u30BF\
  \u306E\u4FDD\u5B58\u3068\u3044\u3063\u305F\u7406\u7531\u304B\u3089\u884C\u308F\u308C\
  \u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:41.515136-06:00'
model: gpt-4-1106-preview
summary: "\u30C7\u30FC\u30C8\u3092\u6587\u5B57\u5217\u306B\u5909\u63DB\u3059\u308B\
  \u3068\u306F\u3001\u30D7\u30ED\u30B0\u30E9\u30E0\u5185\u306E\u65E5\u4ED8\u30C7\u30FC\
  \u30BF\u3092\u4EBA\u304C\u8AAD\u3081\u308B\u30C6\u30AD\u30B9\u30C8\u5F62\u5F0F\u306B\
  \u3059\u308B\u3053\u3068\u3067\u3059\u3002\u30ED\u30B0\u51FA\u529B\u3001\u30E6\u30FC\
  \u30B6\u30FC\u30A4\u30F3\u30BF\u30FC\u30D5\u30A7\u30A4\u30B9\u3001\u30C7\u30FC\u30BF\
  \u306E\u4FDD\u5B58\u3068\u3044\u3063\u305F\u7406\u7531\u304B\u3089\u884C\u308F\u308C\
  \u307E\u3059\u3002"
title: "\u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\u5909\u63DB\u3059\u308B"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
デートを文字列に変換するとは、プログラム内の日付データを人が読めるテキスト形式にすることです。ログ出力、ユーザーインターフェイス、データの保存といった理由から行われます。

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
