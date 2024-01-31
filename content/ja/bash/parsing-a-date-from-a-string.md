---
title:                "文字列から日付を解析する"
date:                  2024-01-20T15:34:34.939427-07:00
html_title:           "Arduino: 文字列から日付を解析する"
simple_title:         "文字列から日付を解析する"

category:             "Bash"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
日付を文字列から解析するとは、テキスト形式の日付を扱える形に変換することです。プログラマーは、ユーザーの入力やデータファイルから日付データを得て操作するためにこれを行います。

## How to: (方法)
Bashで日付を解析するには、`date` コマンドと `+` オプションを使います。以下は具体的な例です。

```Bash
# ISO 8601 形式の日付を解析する
date_input="2023-03-14T15:53:00"
parsed_date=$(date -d "$date_input" '+%Y-%m-%d %H:%M:%S')
echo $parsed_date

# 出力: 2023-03-14 15:53:00
```

```Bash
# 日本の日付形式を解析してみます
date_jp="2023年03月14日 15時53分"
parsed_date_jp=$(date -d "$date_jp" '+%Y-%m-%d %H:%M:%S')
echo $parsed_date_jp

# 出力: date: invalid date ‘2023年03月14日 15時53分’
# Bashの 'date' コマンドはこの形式を標準で解析できません
```

## Deep Dive (深い潜水)
Bashの `date` コマンドは1970年代からあるUNIXコマンドの一つで、時刻や日付の扱いには長い歴史があります。ただし、このコマンドは言語や地域設定に強く依存し、特定の日付形式だけを解析できます。ある国の日付形式を解析するためには、場合によっては`sed` や `awk` などのツールで前処理を行う必要があることもあります。

```Bash
# 日本の日付形式を解析するための前処理例
date_jp="2023年03月14日 15時53分"
parsed_date_jp=$(date -d "$(echo $date_jp | sed -E 's/年|-|月/-/g; s/日//g; s/時/:/g; s/分//g')" '+%Y-%m-%d %H:%M:%S')
echo $parsed_date_jp

# 出力: 2023-03-14 15:53:00
```

他の言語では、専用のライブラリや関数が用意されており、日付の解析がより柔軟に行えます。例えば、Pythonには `dateutil` ライブラリがあります。

## See Also (関連項目)
- GNU Coreutilsの `date` マニュアル: https://www.gnu.org/software/coreutils/manual/coreutils.html#date-invocation
- Advanced Bash-Scripting Guide: https://www.tldp.org/LDP/abs/html/
- `date` コマンドに関する詳細な例と説明: https://linuxize.com/post/date-command-in-linux/
