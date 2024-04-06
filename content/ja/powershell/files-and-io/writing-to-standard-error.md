---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:31.546558-07:00
description: "\u65B9\u6CD5: PowerShell \u306F `Write-Error` \u30B3\u30DE\u30F3\u30C9\
  \u30EC\u30C3\u30C8\u3092\u4F7F\u7528\u3059\u308B\u304B\u3001\u51FA\u529B\u3092 `$host.ui.WriteErrorLine()`\
  \ \u30E1\u30BD\u30C3\u30C9\u306B\u5411\u3051\u308B\u3053\u3068\u306B\u3088\u3063\
  \u3066\u3001stderr \u3078\u306E\u66F8\u304D\u8FBC\u307F\u30D7\u30ED\u30BB\u30B9\u3092\
  \u7C21\u7D20\u5316\u3057\u307E\u3059\u3002\u305F\u3060\u3057\u3001\u76F4\u63A5\u306E\
  \ stderr \u30EA\u30C0\u30A4\u30EC\u30AF\u30C8\u306B\u306F\u3001.NET \u30E1\u30BD\
  \u30C3\u30C9\u3084\u2026"
lastmod: '2024-04-05T22:38:41.967237-06:00'
model: gpt-4-0125-preview
summary: "PowerShell \u306F `Write-Error` \u30B3\u30DE\u30F3\u30C9\u30EC\u30C3\u30C8\
  \u3092\u4F7F\u7528\u3059\u308B\u304B\u3001\u51FA\u529B\u3092 `$host.ui.WriteErrorLine()`\
  \ \u30E1\u30BD\u30C3\u30C9\u306B\u5411\u3051\u308B\u3053\u3068\u306B\u3088\u3063\
  \u3066\u3001stderr \u3078\u306E\u66F8\u304D\u8FBC\u307F\u30D7\u30ED\u30BB\u30B9\u3092\
  \u7C21\u7D20\u5316\u3057\u307E\u3059\u3002\u305F\u3060\u3057\u3001\u76F4\u63A5\u306E\
  \ stderr \u30EA\u30C0\u30A4\u30EC\u30AF\u30C8\u306B\u306F\u3001.NET \u30E1\u30BD\
  \u30C3\u30C9\u3084 PowerShell \u81EA\u4F53\u304C\u63D0\u4F9B\u3059\u308B\u30D5\u30A1\
  \u30A4\u30EB\u30C7\u30A3\u30B9\u30AF\u30EA\u30D7\u30BF\u306E\u30EA\u30C0\u30A4\u30EC\
  \u30AF\u30B7\u30E7\u30F3\u3092\u597D\u3080\u304B\u3082\u3057\u308C\u307E\u305B\u3093\
  \u3002"
title: "\u6A19\u6E96\u30A8\u30E9\u30FC\u3078\u306E\u66F8\u304D\u8FBC\u307F"
weight: 25
---

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
