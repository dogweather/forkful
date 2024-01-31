---
title:                "テキストファイルの書き込み"
date:                  2024-01-19
html_title:           "Bash: テキストファイルの書き込み"
simple_title:         "テキストファイルの書き込み"

category:             "PowerShell"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
テキストファイルを書くとは、データをテキスト形式で保存することです。プログラマーはログ情報の保存、設定の保存、またはデータ交換のためにこれを行います。

## How to: (方法)
```PowerShell
# ファイルへの単純なテキストの書き込み
"こんにちは、世界！" | Out-File -FilePath "hello.txt"

# ファイルの読み込みと表示
Get-Content -Path "hello.txt"
```
出力:
```
こんにちは、世界！
```
```PowerShell
# ファイルへの追加書き込み
"さようなら、世界" | Out-File -FilePath "hello.txt" -Append

# 再度ファイルの読み込みと表示
Get-Content -Path "hello.txt"
```
出力:
```
こんにちは、世界！
さようなら、世界
```

## Deep Dive (深掘り)
PowerShellでは`Out-File`や`Set-Content`コマンドレットを使ってファイルに書き込むことができます。`Out-File`は特に出力をファイルにリダイレクトしたい場合に使われます。歴史的には、以前のバージョンのPowerShellや他のスクリプト言語ではリダイレクト演算子 (`>` や `>>`) も使用されました。代わりに`.NET`のクラスを使って書き込む方法 (`[System.IO.File]::WriteAllText()` など) もありますが、簡単なタスクではPowerShellのコマンドレットの方が直感的です。

## See Also (関連情報)
- [Out-File コマンドレットの公式ドキュメント](https://docs.microsoft.com/ja-jp/powershell/module/microsoft.powershell.utility/out-file)
- [Get-Content コマンドレットの公式ドキュメント](https://docs.microsoft.com/ja-jp/powershell/module/microsoft.powershell.management/get-content)
- [PowerShell スクリプト基本ガイド](https://docs.microsoft.com/ja-jp/powershell/scripting/overview)
- [.NET の System.IO.File クラス](https://docs.microsoft.com/ja-jp/dotnet/api/system.io.file)
