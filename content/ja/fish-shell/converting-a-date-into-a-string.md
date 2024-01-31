---
title:                "日付を文字列に変換する"
date:                  2024-01-20T17:36:26.486971-07:00
model:                 gpt-4-1106-preview
simple_title:         "日付を文字列に変換する"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)

日付から文字列への変換は、特定の形式の日付データをわかりやすいテキスト形式にすることです。プログラマーはこの変換を、ログ、レポート作成、またはユーザーインターフェイスでの日付表示のために行います。

## How to: (方法：)

```Fish Shell
# 現在の日付と時刻を取得し、独自の形式で出力
set current_date (date)
echo $current_date

# 日付を YYYY-MM-DD 形式で出力
set formatted_date (date "+%Y-%m-%d")
echo $formatted_date
```

Sample Output:
```
Sun Mar 14 15:02:56 JST 2021
2021-03-14
```

## Deep Dive (深掘り)

過去、Unix/Linux 系のシェルでは日付と時刻の操作に `date` コマンドが使われてきました。Fish Shell でもこのコマンドを利用し、日付の文字列変換が可能です。他のプログラミング言語にも同様の機能がありますが、シェルスクリプトでは直接的な方法で扱えるのが魅力です。Fish では日付データを +オプションと組み合わせて、多様なフォーマットに対応することができます。

例えば：
- `%Y`: 年を4桁で表示。
- `%m`: 月を2桁で表示。
- `%d`: 日を2桁で表示。

これにより、ログファイルのタイムスタンプや、プログラムの出力を整形する際、柔軟性と読みやすさを提供します。

## See Also (関連項目)

- Fish Shell 公式ドキュメント: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
- `date` コマンドのマニュアルページ: [https://man7.org/linux/man-pages/man1/date.1.html](https://man7.org/linux/man-pages/man1/date.1.html)
- POSIX `strftime` フォーマット指定子: [https://pubs.opengroup.org/onlinepubs/9699919799/functions/strftime.html](https://pubs.opengroup.org/onlinepubs/9699919799/functions/strftime.html)
