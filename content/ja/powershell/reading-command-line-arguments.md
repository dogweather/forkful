---
title:                "「コマンドライン引数の読み取り」"
html_title:           "PowerShell: 「コマンドライン引数の読み取り」"
simple_title:         "「コマンドライン引数の読み取り」"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 何ができるの？ 
コマンドライン引数の読み込みとは、プログラマーがスクリプトに与えられたコマンドラインの情報を解釈することです。これにより、スクリプトを柔軟に実行することができます。 

## 方法: 
```PowerShell
#コマンドライン引数を処理する方法
param(
    [string]$arg1,
    [int]$arg2
)

#実行コマンド例
.\script.ps1 -arg1 "Hello" -arg2 123

#出力結果
$arg1 = "Hello"
$arg2 = 123
```

## 深い掘り下げ: 
コマンドライン引数の読み取りは、古くからあるプログラミングの基本的な機能です。他にも、環境変数やコマンドラインオプションを使用してプログラムをパラメーターでカスタマイズする方法もあります。実装の詳細については、PowerShellのドキュメンテーションを参照してください。 

## 関連リンク: 
- [PowerShellドキュメンテーション](https://docs.microsoft.com/ja-jp/powershell/)
- [コマンドラインオプションの使用方法](https://docs.microsoft.com/ja-jp/powershell/module/microsoft.powershell.core/about/about_powershell?view=powershell-7.1#parameters)