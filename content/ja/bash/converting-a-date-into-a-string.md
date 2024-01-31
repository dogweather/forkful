---
title:                "日付を文字列に変換する"
date:                  2024-01-20T17:35:54.654331-07:00
model:                 gpt-4-1106-preview
simple_title:         "日付を文字列に変換する"

category:             "Bash"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
日付を文字列に変換するとは、例えば年月日をテキスト形式に直すことです。ログファイルのタイムスタンプやレポートの日付表示などに使われます。

## How to: (やり方)
```Bash
# 現在の日付を YYYY-MM-DD 形式で取得
date_str=$(date '+%Y-%m-%d')
echo $date_str

# 出力例: 2023-03-15
```

```Bash
# 特定のフォーマットで日付を設定 (例: 日本の元号を使う)
jp_date_str=$(date '+令和%y年%m月%d日')
echo $jp_date_str

# 出力例: 令和5年03月15日
```

## Deep Dive (詳細情報)
日付を文字列に変換する必要性はコンピュータ初期からありました。UNIX系OSでは、`date` コマンドがこれを行う標準的なツールです。別のオプションにはPythonやPerlなどのスクリプト言語があり、多様なフォーマットオプションが用意されています。Bashでの実装も柔軟性が高いですが、注意点としては、シェルや地域によって `date` コマンドの挙動が異なることがあります。例えば、macOSの `date` コマンドは、Linuxで使うものとオプションが異なることがあります。シェルスクリプトを書くときは、互換性を意識して書くことが重要です。

## See Also (関連情報)
- GNU Coreutils `date` マニュアル: https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html
- Advanced Bash-Scripting Guide: https://www.tldp.org/LDP/abs/html/
- 日本時刻に関する更なる情報: https://www.nict.go.jp/JST/JST5.html
