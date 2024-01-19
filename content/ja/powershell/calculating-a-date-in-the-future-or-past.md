---
title:                "未来または過去の日付の計算"
html_title:           "PowerShell: 未来または過去の日付の計算"
simple_title:         "未来または過去の日付の計算"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 何となぜ？
日付を未来や過去に計算するとは、特定の日付から特定の期間（例えば5日後や2年前など）を計算することを指します。プログラマはこの計算を行うことで、特定の期間後の日付を取得したり、日付の差を計算したりします。

## 方法：
具体的なコード例と出力結果を示します。

```PowerShell
# 今日の日付を表示
$Today = Get-Date
echo "今日の日付: $Today "

# 5日後の日付を計算
$FutureDate = $Today.AddDays(5)
echo "５日後の日付: $FutureDate "

# 2年前の日付を計算
$PastDate = $Today.AddYears(-2)
echo "２年前の日付: $PastDate "
```
以上のコードを実行すると、以下のような出力結果が得られます。

```PowerShell
今日の日付: 2022-03-30 09:15:23
５日後の日付: 2022-04-04 09:15:23
２年前の日付: 2020-03-30 09:15:23
```
## ディープダイブ：
日付計算は時代を問わずあらゆるプログラミング言語で日々使われています。





PowerShellでは、.Net Framework提供の機能を使って日付計算を行います。それには `DateTime` クラスとそのメソッド (`AddDays`, `AddYears`等) が有利に使われます。

他の代替方法として、[System.DateTimeOffset]クラスもありますが、日付/時刻の計算処理では[`DateTime`]クラスが一般的です。

詳細な実装では、日付計算はグレゴリオ暦に基づいて行われます。ただし、特殊なカレンダー（ヒジュラ暦やユダヤ暦など）を使用するためのAPIも.NET Frameworkには用意されています。

## 参考資料：
以下のリンクは関連するリソースで、より深く学ぶためのものです。

- PowerShellによる日付計算： https://ss64.com/ps/datetime.html
- DateTimeクラスについて： https://docs.microsoft.com/ja-jp/dotnet/api/system.datetime
- DateTimeOffsetクラスについて： https://docs.microsoft.com/ja-jp/dotnet/api/system.datetimeoffset
- グレゴリオ暦について： https://docs.microsoft.com/ja-jp/dotnet/api/system.globalization.gregoriancalendar