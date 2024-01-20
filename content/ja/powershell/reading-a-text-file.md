---
title:                "テキストファイルの読み込み"
html_title:           "Bash: テキストファイルの読み込み"
simple_title:         "テキストファイルの読み込み"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 何となぜ？

テキストファイル読み込みとは、プログラムがテキストファイル内のデータを読む作業のことです。これは設定情報やデータ分析、ログチェックなど、プログラムが情報を得る主要な手段の一つであるからです。

## 実装方法：

以下にPowerShellでテキストファイルを読み込む方法を示します。

```PowerShell
$filePath = "C:\Users\UserName\Documents\Sample.txt"
$content = Get-Content $filePath
$content
```

上記のコードは`Sample.txt`ファイルの内容を読み込み、その内容を表示します。

```PowerShell 
John Doe
Jane Doe
```

上記は出力例で、 `Sample.txt`ファイルに `John Doe`と `Jane Doe`が含まれていた場合のものです。

## 詳細情報：

テキストファイルの扱いはプログラムの基本であり、PowerShellも長い歴史を持つ言語であるため、この機能は初期から実装されています。また、Get-Contentと類似の機能を持つコマンドとしては、`type`や `cat`があります。

具体的な実装については、Get-Contentは遅延読み込みを行う特性があります。つまり、大きなファイルを読み込む際でも、必要な部分だけを読み込んでプログラムのパフォーマンスを保つことができます。

## 参考資料：

- [MSDN公式ドキュメンテーション: Get-Content](https://docs.microsoft.com/ja-jp/powershell/module/microsoft.powershell.management/get-content?view=powershell-7.1)
- [PowerShellでファイルの内容を読み込む方法](https://www.red-gate.com/simple-talk/sysadmin/powershell/powershell-day-to-day-admin-tasks-reading-text-files/)
- [聖書 - PowerShellガイド：ファイルとディレクトリ](http://www.vivekanantha.net/powershell/powershell-guide-to-files-and-folders/)