---
title:                "コマンドライン引数の読み取り"
html_title:           "Bash: コマンドライン引数の読み取り"
simple_title:         "コマンドライン引数の読み取り"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 何となぜ？

コマンドライン引数の読み取りとは、スクリプトを起動時に外部から情報を渡す方法です。プログラマーは、動的な動作変更や複数のタスクを一度に実行するためにこれを行います。

## 方法:

以下は基本的なコマンドライン引数の読み取りの例です：

```PowerShell
param (
    [Parameter(Mandatory=$true)]
    [string] $Name,

    [Parameter(Mandatory=$false)]
    [string] $Location
)
Write-Output "Name: $Name"
Write-Output "Location: $Location"
```
スクリプトを以下のように呼び出す場合、出力は次のようになります：
```PowerShell
.\myscript.ps1 -Name "Tanaka" -Location "Tokyo"
```
出力:
```PowerShell
Name: Tanaka
Location: Tokyo
```
## ディープダイブ

コマンドライン引数を読むという概念は、PowerShell問わずプログラミングの多くの領域で存在します。これは、スクリプトやプログラムが外部からユーザーの入力を受け取ったり、一連の操作を自動化したりするために必要です。

別の方法としては `$args` 配列を使う方法がありますが、`param` ブロックを使用すれば、引数の型や必須条件を明示的に定義できますので、品質管理が可能になります。

変数 `Name` と `Location` は、関数の内部で自由に使用することができます。これらはスクリプトの開始時に引数として渡されます。

## 参考資料

コマンドライン引数の詳細については以下のリンクをご覧ください：

- [About Parameters](https://docs.microsoft.com/ja-jp/powershell/scripting/language/cmdlets/parameters?view=powershell-7.1)
- [About Functions](https://docs.microsoft.com/ja-jp/powershell/scripting/developer/cmdlet/about-functions-cmdlet-parameters?view=powershell-7.1)
- [$args](https://docs.microsoft.com/ja-jp/powershell/module/microsoft.powershell.core/about/about_automatic_variables?view=powershell-7.1#$args)