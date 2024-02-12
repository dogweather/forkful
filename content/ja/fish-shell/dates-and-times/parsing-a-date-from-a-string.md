---
title:                "文字列から日付をパースする"
aliases: - /ja/fish-shell/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:14:19.097332-07:00
model:                 gpt-4-0125-preview
simple_title:         "文字列から日付をパースする"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？
文字列から日付を解析するとは、文字列内にエンコードされた日付情報を抽出し、プログラミング環境が認識して操作できる構造化された形式に変換することを指します。プログラマーは、日付の比較、算術、フォーマット、ローカリゼーションなどの操作を可能にするためにこれを行います。これらは、スケジューリング、タイムスタンプ、履歴データをソフトウェアで効率的に扱う上で不可欠です。

## 方法：
Fish Shellでは、文字列から日付を解析するために特別に設計された組み込みコマンドはありません。代わりに、`date`（LinuxおよびmacOSで利用可能）のような外部ユーティリティに依存するか、より複雑な解析に向いている`GNU date`のような人気のサードパーティツールを活用します。以下がそのアプローチです：

**Fishで`date`を使用する：**

"YYYY-MM-DD"の形式の日付文字列を解析するには、`date`コマンドを`-d`（またはGNU dateでは`--date`）オプションに続けて文字列を指定して使用します。出力形式をフォーマットするには`+`オプションを使用します。

```fish
set date_str "2023-04-01"
date -d $date_str +"%A, %d %B %Y"
# 出力: Saturday, 01 April 2023
```

macOS用（`-j`と`-f`フラグに異なる形式が必要です）：

```fish
set date_str "2023-04-01"
date -j -f "%Y-%m-%d" $date_str +"%A, %d %B %Y"
# 出力: Saturday, 01 April 2023
```

**複雑な解析のためのGNU `date`の使用：**

GNU `date`は文字列形式に対してより柔軟です。入力形式を明示的に指定せずに、多くの一般的な日付文字列形式を自動的に検出することができます：

```fish
set complex_date_str "April 1, 2023 14:00"
date -d "$complex_date_str" '+%Y-%m-%d %H:%M:%S'
# 出力: 2023-04-01 14:00:00
```

ただし、自動的に認識されないかもしれない日付文字列を扱う場合や、入力形式について厳密な制御が必要な場合には、GNU `date`で入力形式を指定することは直接サポートされていません。そのような場合は、文字列を事前処理するか、より複雑な日付解析ルーティンのために設計された別のツールを使用することを検討してください。
