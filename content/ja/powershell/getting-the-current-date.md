---
title:                "「現在の日付を取得する」"
html_title:           "PowerShell: 「現在の日付を取得する」"
simple_title:         "「現在の日付を取得する」"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 今何&なぜ？

現在の日付を取得するとは、プログラマーが現在の日付情報を得るための方法です。プログラマーがこれを行う理由は、日付情報がポータブルで再利用可能な方法で取得できるため、時にはプログラムの動作に必要な特定の日付を取得するためです。

## 方法：

```PowerShell
# 現在の日付を取得するコマンドレット
Get-Date

# 現在の日付だけでなく、時刻や曜日も表示する場合
Get-Date -Format "yyyy/MM/dd, ddd HH:mm:ss"

# 別のタイムゾーンで現在の日付を取得する場合
Get-Date -TimeZone "Central Standard Time"

# 変数に現在の日付を格納する
$currentDate = Get-Date
```

出力例：
```
2020/07/22, Wed 14:51:22
```

## 深堀り：

この機能は、 Unixオペレーティングシステムの `date` コマンドや、他のプログラミング言語で使用される `DateTime` クラスと似ています。また、PowerShellでは `.NET` フレームワークの `DateTime` クラスが使用されており、より高度な操作が可能です。

## 参考：

- [Microsoft Docs - Get-Date](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7)
- [TechNet - 日期和時間命令碼](https://docs.microsoft.com/zh-tw/powershell/scripting/scripts/date-and-time/overview-of-date-and-time-commands-in-powershell)