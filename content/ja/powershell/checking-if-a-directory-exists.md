---
title:                "ディレクトリが存在するかどうかの確認"
html_title:           "Gleam: ディレクトリが存在するかどうかの確認"
simple_title:         "ディレクトリが存在するかどうかの確認"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 何となぜ？

ディレクトリが存在しているかを確認するとは、文字通り指定したディレクトリが存在しているかどうかを調べることです。プログラマーがこれを行う理由は、スクリプトが正常に動作するために特定のディレクトリが必要であるため、そのディレクトリが存在することを確認する必要があるからです。

## やり方：

以下のPowerShellコードでディレクトリが存在するかを確認することができます。

```PowerShell
$dirPath = 'C:\your\directory\path'
if (Test-Path $dirPath) {
  Write-Output "$dirPath exists."
} else {
  Write-Output "$dirPath does not exist."
}
```

これを実行すると、指定したディレクトリが存在する場合は `C:\your\directory\path exists.` と表示され、存在しない場合は `C:\your\directory\path does not exist.` と表示されます。

## ディープダイブ：

ディレクトリ存在確認の手法は、Unix系システムのような他のオペレーティングシステムで長年にわたり使われてきました。PowerShellでは`Test-Path` cmdletを使いますが、Unix系システムでは`[ -d $DIR ]`といった形で行います。

代替手段としては、次のコマンドを使ってディレクトリの存在を確認することもできます。

``` PowerShell
$dirPath = 'C:\your\directory\path'
(Get-Item $dirPath -ErrorAction SilentlyContinue).PSIsContainer
```

ただし、このコマンドは $dirPath が存在しないときに False を返し、存在してディレクトリでないときも False を返すため、ディレクトリの存在だけを確認したい場合は `Test-Path` の使用が推奨されます。

## 参考資料：

- [PowerShell `Test-Path` documentation](https://docs.microsoft.com/ja-jp/powershell/module/microsoft.powershell.management/test-path) - ディレクトリ存在確認の基本的な方法について説明している公式のドキュメンテーション。
- [Unix Test Command](https://www.tutorialspoint.com/unix_commands/test.htm) - Unix系システムにおけるディレクトリ存在確認の背景や手法について解説。