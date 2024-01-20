---
title:                "文字列から日付を解析する"
html_title:           "Bash: 文字列から日付を解析する"
simple_title:         "文字列から日付を解析する"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ？

文字列から日付を解析することは、日付情報を文字列から日付オブジェクトに変換するプロセスです。プログラマーがこれを行うのは、日付関連の操作（並べ替え、比較、計算など）を行うためです。

## 方法：

PowerShellで日付解析を行う例を以下に示します。

```PowerShell
$strDate = '2022/05/11'
$date = [DateTime]::Parse($strDate)
```
そして、出力は次のようになります：
```PowerShell
Wednesday, May 11, 2022 12:00:00 AM
```
## ディープダイブ：

1. **歴史的なコンテキスト**： PowerShellは2006年に発表され、そのパワフルな文字列解析能力により評価を受けてきました。
2. **代替案**： PowerShellの 'ParseExact' メソッドを使うことも可能です。これは、明示的な日付/時間形式を指定して解析を行う場合に役立ちます。
3. **実装の詳細**： PowerShellの[DateTime]::Parse メソッドは、与えられた文字列を現在のカルチャ設定に基づいて日付と時間に解析します。これは、カルチャによって日付の書式が異なるため、特に重要です。

```PowerShell
$strDate = '11/5/2022'
$format = 'd/M/yyyy'
$culture = [Globalization.CultureInfo]::InvariantCulture
$date = [DateTime]::ParseExact($strDate, $format, $culture)
```

そして、出力は次のようになります：

```PowerShell
Saturday, May 11, 2022 12:00:00 AM
```

## 参照資料：

- [Microsoft公式ドキュメンテーション：DateTime.Parse メソッド](https://docs.microsoft.com/ja-jp/dotnet/api/system.datetime.parse?view=net-5.0)

以上で日付の解析に関する解説を終わります。