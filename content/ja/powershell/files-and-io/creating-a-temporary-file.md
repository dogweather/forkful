---
date: 2024-01-20 17:41:17.282907-07:00
description: "\u4E00\u6642\u30D5\u30A1\u30A4\u30EB\u306F\u77ED\u671F\u9593\u30C7\u30FC\
  \u30BF\u3092\u4FDD\u5B58\u3059\u308B\u305F\u3081\u306B\u4F5C\u3089\u308C\u3001\u30E1\
  \u30E2\u30EA\u3068\u30C7\u30A3\u30B9\u30AF\u306E\u9593\u3067\u4F5C\u696D\u3092\u52B9\
  \u7387\u5316\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u30C7\
  \u30FC\u30BF\u306E\u4E00\u6642\u4FDD\u7BA1\u3001\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\
  \u30E7\u30F3\u9593\u306E\u30C7\u30FC\u30BF\u8EE2\u9001\u3001\u3042\u308B\u3044\u306F\
  \u4F5C\u696D\u4E2D\u306E\u30C7\u30FC\u30BF\u30D0\u30C3\u30AF\u30A2\u30C3\u30D7\u306E\
  \u305F\u3081\u306B\u4F7F\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.464373-06:00'
model: gpt-4-1106-preview
summary: "\u4E00\u6642\u30D5\u30A1\u30A4\u30EB\u306F\u77ED\u671F\u9593\u30C7\u30FC\
  \u30BF\u3092\u4FDD\u5B58\u3059\u308B\u305F\u3081\u306B\u4F5C\u3089\u308C\u3001\u30E1\
  \u30E2\u30EA\u3068\u30C7\u30A3\u30B9\u30AF\u306E\u9593\u3067\u4F5C\u696D\u3092\u52B9\
  \u7387\u5316\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u30C7\
  \u30FC\u30BF\u306E\u4E00\u6642\u4FDD\u7BA1\u3001\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\
  \u30E7\u30F3\u9593\u306E\u30C7\u30FC\u30BF\u8EE2\u9001\u3001\u3042\u308B\u3044\u306F\
  \u4F5C\u696D\u4E2D\u306E\u30C7\u30FC\u30BF\u30D0\u30C3\u30AF\u30A2\u30C3\u30D7\u306E\
  \u305F\u3081\u306B\u4F7F\u3044\u307E\u3059\u3002."
title: "\u4E00\u6642\u30D5\u30A1\u30A4\u30EB\u306E\u4F5C\u6210"
weight: 21
---

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
