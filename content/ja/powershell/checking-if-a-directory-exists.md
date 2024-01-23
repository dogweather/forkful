---
title:                "ディレクトリが存在するかどうかの確認"
date:                  2024-01-20T14:58:18.934788-07:00
html_title:           "Gleam: ディレクトリが存在するかどうかの確認"
simple_title:         "ディレクトリが存在するかどうかの確認"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (何とその理由？)

ディレクトリが存在するかを確認することは、ファイルシステムにおいて特定の場所にフォルダがあるかを調べる作業です。プログラマは、データを保存したり、ファイルを読み込んだりする前にこれを行い、エラーを避け、プログラムの信頼性を高めます。

## How to: (方法：)

```PowerShell
# 例1: Test-Pathコマンドレットを使用
$directoryPath = "C:\example\path"
$directoryExists = Test-Path $directoryPath
Write-Host "ディレクトリの存在：$directoryExists"

# 出力例:
ディレクトリの存在：True
```

```PowerShell
# 例2: [System.IO.Directory]クラスを使用
$directoryPath = "C:\another\example\path"
$directoryExists = [System.IO.Directory]::Exists($directoryPath)
Write-Host "ディレクトリの存在：$directoryExists"

# 出力例:
ディレクトリの存在：False
```

## Deep Dive (深掘り)

PowerShellでは、ディレクトリの存在を確認する伝統的な方法は`Test-Path`コマンドレットを使うことです。これはPowerShellの初期バージョンから存在し、最も一般的に使われています。または`.NET`の `[System.IO.Directory]::Exists()` メソッドも同じ機能を提供し、これらの間でパフォーマンスの違いはほとんど無いですが、`.NET`の方が.NETを広く使っているプログラマにはなじみ深いかもしれません。

`Test-Path`はよりPowerShellの流儀に沿っていて、パイプラインと一緒によく使われます。`[System.IO.Directory]::Exists()`はオブジェクト指向のスタイルで、より複雑な.NETプログラミングで使われることが多いです。

広い観点では、ディレクトリが存在するかどうかを確認することは、リソースが利用可能かどうかを検証し、スクリプトが実行される環境を管理する上で重要です。プログラムの妥当性を維持するため、このような検査は避けては通れない道です。

## See Also (関連情報)

- [about_Test-Path (公式ドキュメント)](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/test-path)
- [System.IO.Directory.Exists メソッド (公式ドキュメント)](https://docs.microsoft.com/en-us/dotnet/api/system.io.directory.exists)
- [PowerShell Gallery (スクリプトライブラリ)](https://www.powershellgallery.com/)
