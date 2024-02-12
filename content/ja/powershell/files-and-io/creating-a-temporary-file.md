---
title:                "一時ファイルの作成"
aliases:
- /ja/powershell/creating-a-temporary-file.md
date:                  2024-01-20T17:41:17.282907-07:00
model:                 gpt-4-1106-preview
simple_title:         "一時ファイルの作成"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
一時ファイルは短期間データを保存するために作られ、メモリとディスクの間で作業を効率化します。プログラマーはデータの一時保管、アプリケーション間のデータ転送、あるいは作業中のデータバックアップのために使います。

## How to: (実践方法)
```PowerShell
# 一時ファイルを作る
$tempFile = [System.IO.Path]::GetTempFileName()

# 作成された一時ファイルのパスを表示
Write-Output $tempFile

# 何かデータを一時ファイルへ書き込む
"サンプルテキスト" | Out-File -FilePath $tempFile

# 一時ファイルの内容を読み取る
Get-Content -Path $tempFile
```
サンプル出力：
```
C:\Users\[ユーザー名]\AppData\Local\Temp\tmpE617.tmp
サンプルテキスト
```

## Deep Dive (深掘り)
過去、一時ファイルはメインメモリの限られた容量に対処するために使われました。今日、`[System.IO.Path]::GetTempFileName()`のような.NETメソッドが一時ファイルを安全に作るための標準手法です。代替案には`New-TemporaryFile`コマンドレットや`mktemp`（Linux系の環境）があります。これらのツールは、衝突を避けファイルの重複を防ぐ一意なファイル名を生成します。

## See Also (関連情報)
- [System.IO.Path クラスドキュメント](https://docs.microsoft.com/en-us/dotnet/api/system.io.path?view=netframework-4.8)
- [New-TemporaryFile コマンドレット](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/new-temporaryfile?view=powershell-7.1)
- `Get-Help Out-File` - PowerShell内のコマンドのヘルプ
- `Get-Help Get-Content` - PowerShell内のコマンドのヘルプ
