---
title:                "現在の日付を取得する"
date:                  2024-01-20T15:16:14.768293-07:00
html_title:           "Bash: 現在の日付を取得する"
simple_title:         "現在の日付を取得する"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (何とその理由？)

PowerShellで現在の日付を取得するのは、システムのタイムスタンプが必要な時、すなわちログの記録やスケジュールされたタスクの実行などに使います。

## How to: (方法：)

```PowerShell
# 現在の日付と時刻を取得
Get-Date

# 出力例
Wednesday, March 31, 2023 14:23:45
```

```PowerShell
# 異なるフォーマットで日付を取得
Get-Date -Format "yyyy/MM/dd HH:mm:ss"

# 出力例
2023/03/31 14:23:45
```

```PowerShell
# 一部の日付要素だけを取得
$date = Get-Date
$date.DayOfWeek

# 出力例
Wednesday
```

## Deep Dive (深掘り)

PowerShellにおける`Get-Date`コマンドは、.NETの`DateTime`クラスをベースにしています。これはWindows PowerShellが初登場した2006年から存在しています。リニューアルされたPowerShell Coreでもその役割は変わりません。

`Get-Date`の提供するオプションを使えば、様々なフォーマットで日付を表示できます。`-Format`パラメータは、カスタムの日付と時刻の文字列を定義するのに使えます。これはログファイルの名前や、ユーザーに対する日付の表示など、特定のフォーマットが求められる局面で便利です。

他の言語やシステムでは、UNIXタイムスタンプやdateコマンドを利用して現在の日付や時刻を取得する方法があります。しかし、PowerShellはWindows環境での作業に特化しており、`Get-Date`はその簡易性からよく使われます。

## See Also (関連情報)

- [PowerShell Documentation - `Get-Date`](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.3)
- [.NET API Browser - `DateTime` Class](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-7.0)
- [Microsoft's PowerShell Scripting](https://docs.microsoft.com/en-us/powershell/scripting/overview?view=powershell-7.3)
