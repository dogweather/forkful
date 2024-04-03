---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:31.546558-07:00
description: "PowerShell \u3067\u306E\u6A19\u6E96\u30A8\u30E9\u30FC\uFF08stderr\uFF09\
  \u3078\u306E\u66F8\u304D\u8FBC\u307F\u306F\u3001\u30A8\u30E9\u30FC\u30E1\u30C3\u30BB\
  \u30FC\u30B8\u3084\u8A3A\u65AD\u60C5\u5831\u3092\u6A19\u6E96\u51FA\u529B\uFF08stdout\uFF09\
  \u30B9\u30C8\u30EA\u30FC\u30E0\u3068\u306F\u5225\u306E stderr\u2026"
lastmod: '2024-03-13T22:44:42.459793-06:00'
model: gpt-4-0125-preview
summary: "PowerShell \u3067\u306E\u6A19\u6E96\u30A8\u30E9\u30FC\uFF08stderr\uFF09\u3078\
  \u306E\u66F8\u304D\u8FBC\u307F\u306F\u3001\u30A8\u30E9\u30FC\u30E1\u30C3\u30BB\u30FC\
  \u30B8\u3084\u8A3A\u65AD\u60C5\u5831\u3092\u6A19\u6E96\u51FA\u529B\uFF08stdout\uFF09\
  \u30B9\u30C8\u30EA\u30FC\u30E0\u3068\u306F\u5225\u306E stderr \u30B9\u30C8\u30EA\
  \u30FC\u30E0\u306B\u76F4\u63A5\u9001\u4FE1\u3059\u308B\u3053\u3068\u3092\u610F\u5473\
  \u3057\u307E\u3059\u3002\u3053\u306E\u5206\u96E2\u306B\u3088\u308A\u3001\u30B9\u30AF\
  \u30EA\u30D7\u30C8\u306E\u51FA\u529B\u3092\u3088\u308A\u6B63\u78BA\u306B\u5236\u5FA1\
  \u3067\u304D\u3001\u958B\u767A\u8005\u306F\u901A\u5E38\u306E\u30E1\u30C3\u30BB\u30FC\
  \u30B8\u3068\u30A8\u30E9\u30FC\u30E1\u30C3\u30BB\u30FC\u30B8\u3092\u7570\u306A\u308B\
  \u5148\u306B\u5411\u3051\u308B\u3053\u3068\u304C\u53EF\u80FD\u306B\u306A\u308A\u3001\
  \u3053\u308C\u306F\u30A8\u30E9\u30FC\u51E6\u7406\u3084\u30ED\u30B0\u8A18\u9332\u306B\
  \u3068\u3063\u3066\u57FA\u672C\u7684\u3067\u3059\u3002."
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
