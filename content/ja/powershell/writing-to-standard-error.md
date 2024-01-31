---
title:                "標準エラーへの書き込み"
date:                  2024-01-19
html_title:           "Arduino: 標準エラーへの書き込み"
simple_title:         "標準エラーへの書き込み"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
標準エラーに書き込むって？プログラマーはなぜそれをするの？
標準エラーへの書き込みは、エラーメッセージや警告を出力するプログラムの機能です。これは、通常の出力とエラー出力を分離しやすくするためや、ログ記録、デバッグで役立ちます。

## How to:
```PowerShell
# 標準エラーへの書き込み例
Write-Error "これはエラーメッセージです。"

# 標準出力と標準エラーの両方への書き込み例
Write-Output "これは通常の出力です。"
Write-Error "そしてこれがエラー出力です。" 2>&1
```
サンプル出力:
```
これは通常の出力です。
Write-Error : そしてこれがエラー出力です。
```

## Deep Dive:
歴史的背景において、標準エラーはUNIXシステムにおける初期のコンセプトで、プログラムには標準入力、標準出力、標準エラーの3つのストリームがあります。PowerShellもこの原則を踏襲しています。選択肢として、`$ErrorActionPreference` や `Try/Catch` ブロックを使用してエラーをより細かく制御できる。実装の詳細では、エラーは `Write-Error` コマンドレットによって `ErrorRecord` オブジェクトとして扱われ、これによりスクリプトのエラーハンドリングが容易になります。

## See Also:
- [about_Redirection](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_redirection)
- [Write-Error](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/write-error)
- [about_Try_Catch_Finally](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_try_catch_finally)
