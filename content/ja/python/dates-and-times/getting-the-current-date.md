---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:49.614694-07:00
description: "Python\u3067\u73FE\u5728\u306E\u65E5\u4ED8\u3092\u53D6\u5F97\u3059\u308B\
  \u3053\u3068\u306F\u3001\u30ED\u30B0\u8A18\u9332\u3001\u30C7\u30FC\u30BF\u5206\u6790\
  \u3001\u6642\u9593\u306B\u57FA\u3065\u304F\u610F\u601D\u6C7A\u5B9A\u306A\u3069\u3001\
  \u591A\u304F\u306E\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u306B\u304A\u3044\
  \u3066\u57FA\u672C\u7684\u306A\u64CD\u4F5C\u3067\u3059\u3002\u3053\u308C\u306F\u3001\
  \u6642\u9593\u7684\u6587\u8108\u306B\u4F9D\u5B58\u3059\u308B\u30BF\u30B9\u30AF\u306B\
  \u4E0D\u53EF\u6B20\u306A\u3001\u30B7\u30B9\u30C6\u30E0\u306E\u73FE\u5728\u306E\u65E5\
  \u4ED8\u3092\u53D6\u5F97\u3059\u308B\u3053\u3068\u306B\u95A2\u4FC2\u3057\u3066\u3044\
  \u307E\u3059\u3002"
lastmod: '2024-03-11T00:14:15.140870-06:00'
model: gpt-4-0125-preview
summary: "Python\u3067\u73FE\u5728\u306E\u65E5\u4ED8\u3092\u53D6\u5F97\u3059\u308B\
  \u3053\u3068\u306F\u3001\u30ED\u30B0\u8A18\u9332\u3001\u30C7\u30FC\u30BF\u5206\u6790\
  \u3001\u6642\u9593\u306B\u57FA\u3065\u304F\u610F\u601D\u6C7A\u5B9A\u306A\u3069\u3001\
  \u591A\u304F\u306E\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u306B\u304A\u3044\
  \u3066\u57FA\u672C\u7684\u306A\u64CD\u4F5C\u3067\u3059\u3002\u3053\u308C\u306F\u3001\
  \u6642\u9593\u7684\u6587\u8108\u306B\u4F9D\u5B58\u3059\u308B\u30BF\u30B9\u30AF\u306B\
  \u4E0D\u53EF\u6B20\u306A\u3001\u30B7\u30B9\u30C6\u30E0\u306E\u73FE\u5728\u306E\u65E5\
  \u4ED8\u3092\u53D6\u5F97\u3059\u308B\u3053\u3068\u306B\u95A2\u4FC2\u3057\u3066\u3044\
  \u307E\u3059\u3002"
title: "\u73FE\u5728\u306E\u65E5\u4ED8\u306E\u53D6\u5F97"
---

{{< edit_this_page >}}

## 何となぜ？

Pythonで現在の日付を取得することは、ログ記録、データ分析、時間に基づく意思決定など、多くのアプリケーションにおいて基本的な操作です。これは、時間的文脈に依存するタスクに不可欠な、システムの現在の日付を取得することに関係しています。

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
