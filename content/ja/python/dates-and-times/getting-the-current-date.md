---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:49.614694-07:00
description: "\u65B9\u6CD5\uFF1A **\u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\u30EA`datetime`\u3092\
  \u4F7F\u7528\u3057\u3066\uFF1A** Python\u306E\u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\
  \u30EA\u306B\u3042\u308B`datetime`\u30E2\u30B8\u30E5\u30FC\u30EB\u306F\u3001\u65E5\
  \u4ED8\u3084\u6642\u523B\u3092\u64CD\u4F5C\u3059\u308B\u305F\u3081\u306E\u30AF\u30E9\
  \u30B9\u3092\u63D0\u4F9B\u3057\u307E\u3059\u3002\u73FE\u5728\u306E\u65E5\u4ED8\u3092\
  \u53D6\u5F97\u3059\u308B\u306B\u306F\u3001`date.today()`\u30E1\u30BD\u30C3\u30C9\
  \u3092\u4F7F\u7528\u3067\u304D\u307E\u3059\u3002"
lastmod: '2024-04-05T22:37:49.847618-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A **\u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\u30EA`datetime`\u3092\
  \u4F7F\u7528\u3057\u3066\uFF1A** Python\u306E\u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\
  \u30EA\u306B\u3042\u308B`datetime`\u30E2\u30B8\u30E5\u30FC\u30EB\u306F\u3001\u65E5\
  \u4ED8\u3084\u6642\u523B\u3092\u64CD\u4F5C\u3059\u308B\u305F\u3081\u306E\u30AF\u30E9\
  \u30B9\u3092\u63D0\u4F9B\u3057\u307E\u3059\u3002\u73FE\u5728\u306E\u65E5\u4ED8\u3092\
  \u53D6\u5F97\u3059\u308B\u306B\u306F\u3001`date.today()`\u30E1\u30BD\u30C3\u30C9\
  \u3092\u4F7F\u7528\u3067\u304D\u307E\u3059\u3002"
title: "\u73FE\u5728\u306E\u65E5\u4ED8\u306E\u53D6\u5F97"
weight: 29
---

## 方法：
**標準ライブラリ`datetime`を使用して：**

Pythonの標準ライブラリにある`datetime`モジュールは、日付や時刻を操作するためのクラスを提供します。現在の日付を取得するには、`date.today()`メソッドを使用できます。

```python
from datetime import date

today = date.today()
print(today)  # 出力: YYYY-MM-DD（例: 2023-04-05）
```

**日付の書式設定：**

異なる形式で現在の日付が必要な場合、`strftime`メソッドを使用してカスタムの日付書式を指定できます：

```python
from datetime import date

today = date.today()
formatted_date = today.strftime('%B %d, %Y')  # 例の形式: "2023年4月5日"
print(formatted_date)
```

**より柔軟性を求める`pendulum`の使用（人気のあるサードパーティライブラリ）：**

`Pendulum`は、Pythonでの日付や時刻の扱いをより直感的にするサードパーティのライブラリです。標準のdatetime機能を拡張し、タイムゾーン管理などの機能を簡素化します。

まず、pip経由で`pendulum`がインストールされていることを確認します：

```shell
pip install pendulum
```

そして、現在の日付を取得するには：

```python
import pendulum

today = pendulum.now().date()
print(today)  # 出力: YYYY-MM-DD（例: 2023-04-05）
```

`Pendulum`を使用すると、書式設定も`strftime`アプローチに似ており、簡単です：

```python
import pendulum

today = pendulum.now()
formatted_date = today.to_formatted_date_string()  # デフォルトの形式: "2023年4月5日"
print(formatted_date)
```
