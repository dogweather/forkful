---
title:                "デバッグ出力の印刷"
html_title:           "Fish Shell: デバッグ出力の印刷"
simple_title:         "デバッグ出力の印刷"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/printing-debug-output.md"
---

{{< edit_this_page >}}

## 何となぜ？

デバッグ出力の印刷は、コードが実行されるたびに特定の情報を出力し、開発者がバグを特定しやすくする、というものです。なぜなら、コードがどのように動作しているかを明らかにするために、それが何をするのか、いつ何をするのかを知る事が必要だからです。

## 使い方：

```PowerShell
$DebugPreference = "Continue"
Write-Debug "This is my debug message"
```

上記のコードでは、デバッグメッセージを有効にして出力を表示しています。出力は次のように表示されます：

```PowerShell
DEBUG: This is my debug message
```

## 深い情報：

PowerShellのデバッグメッセージは、古いバージョンのPowerShellでも利用できる機能で、開発者がプログラムのフローを追跡するのに役立ちます。代替として、Write-HostやWrite-Outputがありますが、これらはユーザー向けの情報出力であって、デバッグ出力ではありません。

デバッグ出力は$DebugPreference変数に依存しています。これはデバッグメッセージの挙動を制御し、'SilentlyContinue'を設定するとデバッグメッセージは抑制され、'Inquire'を設定するとユーザーにメッセージ表示の許可を求めます。

## 参考資料：

以下のリンクを参考にしてください。

1. PowerShellのデバッグ：https://docs.microsoft.com/ja-jp/powershell/scripting/learn/deep-dives/everything-about-debugging?view=powershell-7.1
2. Write-Debugの詳細：https://docs.microsoft.com/ja-jp/powershell/module/microsoft.powershell.utility/write-debug?view=powershell-7.1