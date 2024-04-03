---
date: 2024-01-20 17:35:54.654331-07:00
description: "How to: (\u3084\u308A\u65B9) ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.390574-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\u5909\u63DB\u3059\u308B"
weight: 28
---

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
