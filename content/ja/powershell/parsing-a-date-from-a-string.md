---
title:                "文字列から日付をパースする"
html_title:           "PowerShell: 文字列から日付をパースする"
simple_title:         "文字列から日付をパースする"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 何をするのか？
日付を文字列から解析するとは何かを説明し、プログラマーがそれをする理由を2-3つの文で説明します。

日付を文字列から解析するとは、日付を表す文字列から実際の日付を抽出することを意味します。プログラマーはこれを行うことで、データをより有用な形式に変換し、データの処理や計算をより簡単にすることができます。

## 方法：
```PowerShell
# 例1：日付の解析
[string]$date = "2021/05/23"
$date = [DateTime]::ParseExact($date, "yyyy/MM/dd", $null)
# 出力：2021年5月23日

# 例2：時刻も含まれる場合
[string]$date = "2021/05/23 10:30"
$date = [DateTime]::ParseExact($date, "yyyy/MM/dd hh:mm", $null)
# 出力：2021年5月23日10:30
```

## 深く掘り下げる：
日付を文字列から解析するとは、日付の表現方法が変化したことにより登場しました。以前は日付はプログラムで扱う際に数字のみで表されていましたが、現在では様々な形式で表されるようになりました。このため、日付を文字列から解析する方法が必要になったのです。

他にも、日付を解析する方法としては正規表現を使用する方法や、特定のフォーマットに依存しない汎用的な方法もあります。しかし、PowerShellでは既存の.NETライブラリを利用することで、簡単に日付を文字列から解析することができます。

## 関連情報：
- [PowerShell 公式ドキュメント - DateTime::ParseExactメソッド](https://docs.microsoft.com/ja-jp/dotnet/api/system.datetime.parseexact?view=net-5.0)
- [PowerShell 公式ドキュメント - 日付や時刻の表現方法](https://docs.microsoft.com/ja-jp/dotnet/standard/base-types/custom-date-and-time-format-strings)