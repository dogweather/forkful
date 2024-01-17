---
title:                "一時ファイルを作成する"
html_title:           "PowerShell: 一時ファイルを作成する"
simple_title:         "一時ファイルを作成する"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 何とどうして？

一時ファイルを作成するとは、一時的に使うファイルをコンピューター上に作成することです。プログラマーが一時ファイルを作成する理由は、例えば、一時的にファイルを保存する必要がある場合や、プログラムの処理中に一時ファイルを使用する必要がある場合です。

## 方法：

```PowerShell
# 一時ファイルを作成する
New-TemporaryFile

# 作成した一時ファイルの場所を確認する
Get-TemporaryFile

# ファイル名を指定して一時ファイルを作成する
New-TemporaryFile -Name "example.txt"

# 一時ファイルの内容を表示する
Get-Content (Get-TemporaryFile)

# 一時ファイルを削除する
Remove-TemporaryFile (Get-TemporaryFile)
```

## 詳しい情報：

- 一時ファイルを作成するという概念は、古くから存在しています。昔のコンピューターシステムでは、メモリーが少なく、大きなプログラムを実行することができませんでした。そのため、プログラムを一時的に複数のファイルに分割し、必要に応じて結合して実行する仕組みがありました。
- 一時ファイルを作成する他の方法としては、一時的にフォルダーを作成してそこにファイルを保存する方法があります。しかし、この方法はファイルシステムに余分なファイルやフォルダーを作成してしまうため、一時ファイルを使用する方がより効率的です。
- PowerShellでは、一時ファイルを作成する際には、内部的に.NET Frameworkの ```System.IO.Path.GetTempFileName()``` メソッドを使用しています。このメソッドは、一時ファイルを生成するための一意のファイル名を生成する機能を持っています。

## 関連情報：

- [PowerShellドキュメント：一時ファイルの作成](https://docs.microsoft.com/ja-jp/powershell/module/microsoft.powershell.utility/new-temporaryfile?view=powershell-7)
- [PowerShellドキュメント：一時ファイルの取得](https://docs.microsoft.com/ja-jp/powershell/module/microsoft.powershell.utility/get-temporaryfile?view=powershell-7)
- [PowerShellドキュメント：一時ファイルの削除](https://docs.microsoft.com/ja-jp/powershell/module/microsoft.powershell.utility/remove-temporaryfile?view=powershell-7)