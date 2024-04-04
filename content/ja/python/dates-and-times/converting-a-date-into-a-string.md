---
changelog:
- 2024-04-04, dogweather, edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-01-20 17:37:21.660960-07:00
description: "\u65B9\u6CD5: Python\u3067\u306F\u3001\u65E5\u4ED8\u3092\u6587\u5B57\
  \u5217\u306B\u5909\u63DB\u3059\u308B\u3053\u3068\u304C\u7C21\u5358\u3067\u3059\u3002\
  [date](https://docs.python.org/3/library/datetime.html#date-\u2026"
lastmod: '2024-04-04T02:02:58.383944-06:00'
model: gpt-4-0125-preview
summary: "Python\u3067\u306F\u3001\u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\u5909\
  \u63DB\u3059\u308B\u3053\u3068\u304C\u7C21\u5358\u3067\u3059\u3002[date](https://docs.python.org/3/library/datetime.html#date-objects)\u30AA\
  \u30D6\u30B8\u30A7\u30AF\u30C8\u3067\u5229\u7528\u53EF\u80FD\u306A[`strftime`](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-behavior)\u30E1\
  \u30BD\u30C3\u30C9\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002\u4EE5\u4E0B\u306E\u3088\
  \u3046\u306B\u884C\u3044\u307E\u3059\uFF1A."
title: "\u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\u5909\u63DB\u3059\u308B"
weight: 28
---

## 方法:
Pythonでは、日付を文字列に変換することが簡単です。[date](https://docs.python.org/3/library/datetime.html#date-objects)オブジェクトで利用可能な[`strftime`](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-behavior)メソッドを使用します。以下のように行います：

```Python
from datetime import datetime

# 現在の日付と時刻を取得
now = datetime.now()

# これを '月 日, 年' の形式の文字列に変換
date_string = now.strftime("%B %d, %Y")
print(date_string)  # 出力: March 29, 2023 (または現在の日付)

# 形式: YYYY-MM-DD
iso_date_string = now.strftime("%Y-%m-%d")
print(iso_date_string)  # 出力: 2023-03-29 (または現在の日付)
```


### 私の方法

こちらは、タイムゾーン情報が含まれた[ISO 8601](https://www.w3.org/QA/Tips/iso-date)形式の日付を取得する方法です：

```python
def datestamp() -> str:
    """ 
    タイムゾーンを含むISO形式の現在の日付と時刻。
    """
    return datetime.now().astimezone().isoformat()
```

#### 例出力：

```python
>>> datestamp()
'2024-04-04T01:50:04.169159-06:00'
```



## 深堀り
歴史的に見て、プログラミングにおいて日付の文字列変換は、日付を人が読める形式で表現する必要があるため常に基本でした。

`strftime`の代わりに使用できるものには、ISO 8601形式のための`isoformat`メソッドや、より柔軟なパースとフォーマットオプションを提供する`arrow`や`dateutil`などのサードパーティライブラリがあります。

実装に際して、`strftime`は「string format time」の略であり、C言語に由来しています。Pythonの`strftime`は、年を表す`%Y`や月を表す`%m`などのフォーマットコードを解釈し、ほぼ無限のカスタマイズ可能性を提供します。

## 参照
Pythonの日付と時間の関数についてさらに深掘りするには：
- Python公式の`datetime`ドキュメント: https://docs.python.org/3/library/datetime.html
- `strftime`ディレクティブの包括的なリストに興味のある方へ: https://strftime.org/
- サードパーティの日付/時間ライブラリを探求するには：
  - Arrow: https://arrow.readthedocs.io/en/latest/
  - python-dateutil: https://dateutil.readthedocs.io/en/stable/
