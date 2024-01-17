---
title:                "日付を比較する"
html_title:           "PowerShell: 日付を比較する"
simple_title:         "日付を比較する"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 何を比較するのか？
日付を比較することは、2つの日付を比べてどちらが大きいか小さいかを判断することです。プログラマーは、日付の順番を決めたり、特定の期間に該当するデータを抽出するために日付を比較することがあります。

## 方法：
日付を比較するには、```Compare-Object```コマンドレットを使用します。たとえば、次のようなコードを実行することで、日付を比較することができます。

```PowerShell
$d1 = Get-Date "2020/01/01"
$d2 = Get-Date "2020/01/10"
Compare-Object $d1 $d2
```

実行すると、以下のような結果が表示されます。

```
InputObject                  SideIndicator
-----------                  -------------
1/10/2020 12:00:00 AM        =>
1/1/2020 12:00:00 AM         <=
```

左側の日付が右側の日付よりも大きいことを表す```=>```マークが表示されています。

## 詳細：
日付を比較するために、PowerShellでは```DateTime```オブジェクトというデータ型を使用します。これは、日付と時刻を含むオブジェクトで、そのまま比較することができます。

他の方法としては、単純に日付を文字列として比較する方法がありますが、これは正確性が低く、また処理が遅くなる可能性があります。

また、PowerShellでは日付を数値として扱うこともできます。つまり、日付をシステム上の特定の日からの経過日数として表現することができます。この方法では比較はより早く行うことができますが、日付を扱う際には十分な注意が必要です。

## 関連リンク：
- [Compare-Objectコマンドレットのドキュメント](https://docs.microsoft.com/ja-jp/powershell/module/microsoft.powershell.utility/compare-object)
- [日付と時刻の扱いについて](https://docs.microsoft.com/ja-jp/powershell/scripting/samples/working-with-dates-and-times?view=powershell-7)