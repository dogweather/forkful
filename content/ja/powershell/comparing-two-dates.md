---
title:                "2つの日付を比較する"
html_title:           "Elixir: 2つの日付を比較する"
simple_title:         "2つの日付を比較する"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/comparing-two-dates.md"
---

{{< edit_this_page >}}

---

## 何となんで？ (What & Why?)

日付を比較するとは、ある日付が別の日付よりも前か後かを判断することです。プログラマーは、イベントの順序を確認するためやスケジューリングの問題を解決するために日付を比較します。

## どうやって？ (How to)

```powershell
# 日付を作成します
$date1 = Get-Date -Year 2022 -Month 5 -Day 20
$date2 = Get-Date -Year 2022 -Month 5 -Day 22

# 日付を比較します
if($date1 -gt $date2) {
    echo '日付1の方が新しいです'
}
elseif ($date1 -lt $date2) {
    echo '日付2の方が新しいです'
}
else {
    echo '日付は同じです'
}
```
上記の実行結果は `日付2の方が新しいです` を得ます。

## ディープダイブ (Deep Dive)

- 日付の比較はかねてから計算機科学で重要な課題であり、特にオペレーティングシステムやデータベースで重要な役割を果たしています。PowerShellのような近代的な言語では、この問題を簡単に解決するための組み込み関数を提供しています。
- 代替手段として、[`DateTime.Compare`](https://docs.microsoft.com/ja-jp/dotnet/api/system.datetime.compare?view=net-5.0) メソッドを使って日付を比較することもできます。このメソッドは二つの日付を引数に取り、その結果を基に判断を下します。
- PowerShellでの日付比較は基本的に、.NET Frameworkの `System.DateTime` オブジェクトを使用して行われます。

## 参考文献 (See Also)

- [.NETのDateTimeドキュメンテーション](https://docs.microsoft.com/ja-jp/dotnet/api/system.datetime?view=net-5.0)
- [PowerShellでの日付操作ガイド](https://devblogs.microsoft.com/scripting/working-with-dates-and-times-in-powershell/)