---
title:                "未来または過去の日付を計算する"
html_title:           "Bash: 未来または過去の日付を計算する"
simple_title:         "未来または過去の日付を計算する"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 何となぜ？
過去や未来の日付を計算するとは、特定の日付から特定の期間を加えたり減らしたりすることを指します。この操作はスクリプトが特定の期間後に実行されるタスクをスケジュールするときなどによく使用されます。

## 手順：
以下にbashで未来の日付を計算する例を示します。

```Bash 
 # 3日後の日付を計算
 $ date -d '+3 day'
```

出力例:

```Bash
Tue Oct 29 21:40:13 JST 2021
```

また、過去の日付を計算することもできます。

```Bash 
 # 3日前の日付を計算
 $ date -d '-3 day'
```
出力例:

```Bash
Sat Oct 26 21:40:13 JST 2021
```

## 深層掘り下げ：
### 歴史的文脈:
dateコマンドはUnixの初期から存在し、時間と日付に関する操作を行うためのものです。異なる形式での表示、時間計算などが可能です。

### 選択肢:
Bashだけでなく、PythonやPerlなどの別の言語でも同様の日付計算を行うことができます。各言語は独自の便利な機能を持ち、柔軟性があります。

### 実装詳細:
'-d'オプションは、指定した日付/時刻文字列を解釈します。'+'や'-'記号はそれぞれ未来と過去を指します。

## 参考資料：
以下のリンクは関連する具体的な情報源を提供します。  
- Bash date man page: https://linux.die.net/man/1/date
- DateTime in Python: https://docs.python.org/3/library/datetime.html
- Date in Perl: https://perldoc.perl.org/Time/HiRes.html