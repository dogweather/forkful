---
title:                "現在の日付の取得"
aliases:
- /ja/python/getting-the-current-date/
date:                  2024-02-03T19:10:49.614694-07:00
model:                 gpt-4-0125-preview
simple_title:         "現在の日付の取得"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
