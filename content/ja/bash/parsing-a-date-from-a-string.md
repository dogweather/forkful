---
title:                "文字列から日付をパースする"
aliases:
- ja/bash/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:13:52.479248-07:00
model:                 gpt-4-0125-preview
simple_title:         "文字列から日付をパースする"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？

Bashで文字列から日付を解析することは、テキストデータから日付情報を抽出し、Bashが操作またはさらなるプロセスに使用できる形式に変換することを含みます。これは、ログファイルの分析、日付スタンプに基づくファイルの整理、または自動化された報告などのタスクにおいて、一般的な要件であり、プログラマーが時間データを効果的に管理および活用するための重要なスキルです。

## どのようにして：

Bash自体は直接的な日付解析能力にかなり限定されており、より洗練された操作のために`date`や`awk`のような外部ツールに頻繁に依存しています。ここでは、特定の形式を解析し、その後`date`コマンドを使用して変換したり操作を行ったりする方法を説明します。

**例 1:** 日付文字列を抽出し、別の形式に変換します。

例えば、`yyyy-mm-dd`の形式で日付があり、それを`dd-mm-yyyy`に変換したいとします。

```bash
original_date="2023-04-01"
formatted_date=$(date -d $original_date '+%d-%m-%Y')

echo $formatted_date
```

**サンプル出力:**
```
01-04-2023
```

これは`date`コマンドを`-d`オプションと共に使用して入力日付文字列を指定し、出力をフォーマットするために`+%d-%m-%Y`を使用します。

**例 2:** `awk`を使用して構造化されたテキストラインから日付を解析し、変換します。

ログファイルの行が次のようにあるとします:

```
2023-04-01 12:00:00 ユーザーがログインしました
```

`awk`と`date`を使用して日付部分を抽出し変換することができます。

```bash
log_line="2023-04-01 12:00:00 ユーザーがログインしました"
date_part=$(echo $log_line | awk '{print $1}')
formatted_date=$(date -d $date_part "+%A, %B %d, %Y")

echo $formatted_date
```

**サンプル出力:**
```
土曜日, 4月 01, 2023
```

この例では、`awk`を使用してログラインを分割し、日付部分を抽出します（`$1`は最初のスペースで区切られたフィールドを表します）。その後、`date`を使用してそれを再フォーマットします。

### 第三者ツールの使用

より複雑な解析を行う場合や、さまざまな日付形式を扱う場合、`dateutils`のような第三者ツールが非常に便利です。

**`dateutils`を使用した例:**

例えば、非標準形式の日付文字列、`April 01, 2023`があるとします。

```bash
original_date="April 01, 2023"
formatted_date=$(dateconv -i "%B %d, %Y" -f "%Y-%m-%d" <<< $original_date)

echo $formatted_date
```

**サンプル出力:**
```
2023-04-01
```

このコマンドは、`dateutils`の`dateconv`を使用し、入力形式を`-i`で、求める出力形式を`-f`で指定します。`dateutils`は幅広い日付と時間の形式をサポートしており、Bashスクリプトでの日付解析タスクに非常に多様であることができます。
