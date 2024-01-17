---
title:                "「未来や過去の日付を計算する」"
html_title:           "PowerShell: 「未来や過去の日付を計算する」"
simple_title:         "「未来や過去の日付を計算する」"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 何をするか & なぜするか？
過去や未来の日付を計算することは、プログラマーにとってとても重要です。例えば、あるイベントが開催される日付を計算する場合や、特定の期間後の日付を計算する場合など、日付を計算する必要があるケースは数多くあります。プログラマーたちは、このようなタスクを素早くかつ正確に処理することが求められています。

## 方法：
まずは、``` DateTime ``` オブジェクトを作成します。

```
PowerShell $today = Get-Date 
``` 

次に、``` AddDays() ``` メソッドを使用して、現在の日付から指定した日数を加算することで、未来の日付を計算することができます。

```
PowerShell $futureDate = $today.AddDays(7)
```

同様に、``` AddDays() ``` メソッドの引数に負の値を渡すことで、過去の日付を計算することができます。

```
PowerShell $pastDate = $today.AddDays(-7)
```

## 深堀り：
この日付の計算の仕組みをもう少し深く掘り下げてみましょう。PowerShellでは、タイムゾーン、曜日、特定の月や日を指定することで、さまざまなタイプの日付を計算することができます。

また、``` DateTime ``` オブジェクトを作成する際に、日付フォーマットを指定することもできます。これにより、計算された日付の表示を自分好みの形式に変更することが可能です。

日付を計算する方法は、PowerShellだけではありません。代わりに、``` DateDiff ``` 関数を使用することもできます。これは、2つの日付の間の日数を計算するために使用されます。

## 関連リンク：
- [PowerShell の公式ドキュメント](https://docs.microsoft.com/ja-jp/powershell/scripting/getting-started/using-dates-and-times?view=powershell-6)
- [DateDiff 関数の詳細](https://docs.microsoft.com/ja-jp/dotnet/api/system.datetime.adddays?view=netframework-4.8)
- [C#を使用した日付の計算方法](https://www.techcoil.com/blog/how-to-determine-days-between-two-dates-in-c/javascript-get-days-between-two-dates/)