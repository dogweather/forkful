---
title:                "日付を文字列に変換する"
html_title:           "C++: 日付を文字列に変換する"
simple_title:         "日付を文字列に変換する"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ？
日付を文字列に変換とは、日付のデータ型を文字列のデータ型に変更することです。これはプログラマが人間が直感的に理解しやすい形式で日付を表示したり、グラフィカルなインターフェースに日付を表示したりする場合に重要な作業となります。

## やり方:
次のコードスニペットは、PowerShellを使って現在の日付を`yyyy/MM/dd`形式の文字列に変換する方法を示しています。

```PowerShell
$currentDate = Get-Date
$currentDateString = $currentDate.ToString('yyyy/MM/dd')
Write-Output $currentDateString
```
これを実行すると、次のように出力されます（出力は実行日によります）：

```PowerShell
2023/01/01
```

## 深掘り:
PowerShellは2006年にMicrosoftによって開発され、その強力なスクリプト機能と直感的なシェル操作性からすぐに人気となりました。日付を文字列に変換する機能もその一部です。 

また、日付を異なる形式の文字列に変換することも可能です。たとえば、以下のコードは日付をフルの曜日名と月名を含む形式に変換します。

```PowerShell
$currentDateString = $currentDate.ToString('dddd, MMMM dd, yyyy')
```

実装の詳細については、`.ToString`メソッドとそれに伴う各種カスタム書式文字列についてのドキュメンテーションを参照してください。

## 参考リンク:
以下のリンクでは、このトピックに関してさらに詳細な情報を得ることができます。
1. PowerShellの公式ドキュメンテーション: [https://docs.microsoft.com/ja-jp/powershell/](https://docs.microsoft.com/ja-jp/powershell/)
2. `.ToString`メソッドの詳細: [https://docs.microsoft.com/ja-jp/dotnet/api/system.datetime.tostring](https://docs.microsoft.com/ja-jp/dotnet/api/system.datetime.tostring)
3. PowerShellでの日付と時刻の規則: [https://docs.microsoft.com/ja-jp/powershell/scripting/samples/working-with-dates-and-times](https://docs.microsoft.com/ja-jp/powershell/scripting/samples/working-with-dates-and-times)