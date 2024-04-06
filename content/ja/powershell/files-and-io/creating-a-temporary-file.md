---
date: 2024-01-20 17:41:17.282907-07:00
description: "How to: (\u5B9F\u8DF5\u65B9\u6CD5) \u904E\u53BB\u3001\u4E00\u6642\u30D5\
  \u30A1\u30A4\u30EB\u306F\u30E1\u30A4\u30F3\u30E1\u30E2\u30EA\u306E\u9650\u3089\u308C\
  \u305F\u5BB9\u91CF\u306B\u5BFE\u51E6\u3059\u308B\u305F\u3081\u306B\u4F7F\u308F\u308C\
  \u307E\u3057\u305F\u3002\u4ECA\u65E5\u3001`[System.IO.Path]::GetTempFileName()`\u306E\
  \u3088\u3046\u306A.NET\u30E1\u30BD\u30C3\u30C9\u304C\u4E00\u6642\u30D5\u30A1\u30A4\
  \u30EB\u3092\u5B89\u5168\u306B\u4F5C\u308B\u305F\u3081\u306E\u6A19\u6E96\u624B\u6CD5\
  \u3067\u3059\u3002\u4EE3\u66FF\u6848\u306B\u306F`New-\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:50:56.354338-06:00'
model: gpt-4-1106-preview
summary: "(\u5B9F\u8DF5\u65B9\u6CD5) \u904E\u53BB\u3001\u4E00\u6642\u30D5\u30A1\u30A4\
  \u30EB\u306F\u30E1\u30A4\u30F3\u30E1\u30E2\u30EA\u306E\u9650\u3089\u308C\u305F\u5BB9\
  \u91CF\u306B\u5BFE\u51E6\u3059\u308B\u305F\u3081\u306B\u4F7F\u308F\u308C\u307E\u3057\
  \u305F\u3002\u4ECA\u65E5\u3001`[System.IO.Path]::GetTempFileName()`\u306E\u3088\u3046\
  \u306A.NET\u30E1\u30BD\u30C3\u30C9\u304C\u4E00\u6642\u30D5\u30A1\u30A4\u30EB\u3092\
  \u5B89\u5168\u306B\u4F5C\u308B\u305F\u3081\u306E\u6A19\u6E96\u624B\u6CD5\u3067\u3059\
  \u3002\u4EE3\u66FF\u6848\u306B\u306F`New-TemporaryFile`\u30B3\u30DE\u30F3\u30C9\u30EC\
  \u30C3\u30C8\u3084`mktemp`\uFF08Linux\u7CFB\u306E\u74B0\u5883\uFF09\u304C\u3042\u308A\
  \u307E\u3059\u3002\u3053\u308C\u3089\u306E\u30C4\u30FC\u30EB\u306F\u3001\u885D\u7A81\
  \u3092\u907F\u3051\u30D5\u30A1\u30A4\u30EB\u306E\u91CD\u8907\u3092\u9632\u3050\u4E00\
  \u610F\u306A\u30D5\u30A1\u30A4\u30EB\u540D\u3092\u751F\u6210\u3057\u307E\u3059\u3002"
title: "\u4E00\u6642\u30D5\u30A1\u30A4\u30EB\u306E\u4F5C\u6210"
weight: 21
---

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
