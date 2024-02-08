---
title:                "標準エラーへの書き込み"
aliases:
- ja/powershell/writing-to-standard-error.md
date:                  2024-02-03T19:34:31.546558-07:00
model:                 gpt-4-0125-preview
simple_title:         "標準エラーへの書き込み"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？

PowerShell での標準エラー（stderr）への書き込みは、エラーメッセージや診断情報を標準出力（stdout）ストリームとは別の stderr ストリームに直接送信することを意味します。この分離により、スクリプトの出力をより正確に制御でき、開発者は通常のメッセージとエラーメッセージを異なる先に向けることが可能になり、これはエラー処理やログ記録にとって基本的です。

## 方法:

PowerShell は `Write-Error` コマンドレットを使用するか、出力を `$host.ui.WriteErrorLine()` メソッドに向けることによって、stderr への書き込みプロセスを簡素化します。ただし、直接の stderr リダイレクトには、.NET メソッドや PowerShell 自体が提供するファイルディスクリプタのリダイレクションを好むかもしれません。

**例 1:** `Write-Error` を使用してエラーメッセージを stderr に書き込む。

```powershell
Write-Error "This is an error message."
```

stderr への出力:
```
Write-Error: This is an error message.
```

**例 2:** `$host.ui.WriteErrorLine()` を使用して stderr に直接書き込む。

```powershell
$host.ui.WriteErrorLine("Direct stderr write.")
```

stderr への出力:
```
Direct stderr write.
```

**例 3:** .NET メソッドを使用して stderr に書き込む。

```powershell
[Console]::Error.WriteLine("Using .NET method for stderr")
```

このメソッドの出力:
```
Using .NET method for stderr
```

**例 4:** ファイルディスクリプタ `2>` を使用してエラー出力をリダイレクトする。

PowerShellのファイルディスクリプタは、異なるストリームをリダイレクトできます。stderr の場合、ファイルディスクリプタは `2` です。ここにエラーを生成するコマンドを実行しながら、stderr を `error.log` という名前のファイルにリダイレクトする例を示します。

```powershell
Get-Item NonExistentFile.txt 2> error.log
```

この例はコンソール出力を生成しませんが、存在しないファイルへのアクセスを試みる際のエラーメッセージを含む `error.log` というファイルを現在のディレクトリに生成します。

結論として、PowerShell はエラー出力の効果的な書き込みと管理のための複数の方法を提供し、スクリプトやアプリケーションでの洗練されたエラー処理とログ記録戦略を可能にします。
