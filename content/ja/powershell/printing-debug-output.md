---
title:                "デバッグ出力のプリント"
html_title:           "PowerShell: デバッグ出力のプリント"
simple_title:         "デバッグ出力のプリント"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/printing-debug-output.md"
---

{{< edit_this_page >}}

## 何と何のために？
デバッグ出力をプリントすることは、コードの実行中にプログラマーが確認するための出力です。プログラマーがコードの動作を理解し、不具合を見つけるのに役立ちます。

## 方法：
```PowerShell
Write-Host "デバッグ出力をプリントする方法"
```
このコードを使用して、コンソールにメッセージをプリントすることができます。
```PowerShell
$debug = $true
Write-Host "デバッグモードが有効です" -Debug:$debug
```
「デバッグモードが有効です」というメッセージをプリントし、デバッグモードが有効かどうかを確認することもできます。

## 深く掘り下げる：
デバッグ出力をプリントすることは、バグを見つけるために必要不可欠です。以前は、コードの実行中にメッセージをコンソールに手動でプリントする必要がありましたが、PowerShellではWrite-Hostコマンドレットを使用して簡単に行うことができます。また、PowerShellにはStart-Transcriptコマンドレットを使用して、コンソールに表示されたすべての出力をログファイルに保存する機能があります。

## 参考資料：
- Write-Hostコマンドレットに関する公式ドキュメント: https://docs.microsoft.com/ja-jp/powershell/module/microsoft.powershell.utility/write-host?view=powershell-7
- Start-Transcriptコマンドレットに関する公式ドキュメント: https://docs.microsoft.com/ja-jp/powershell/module/microsoft.powershell.host/start-transcript?view=powershell-7