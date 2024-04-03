---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:20.010966-07:00
description: "\u65B9\u6CD5\uFF1A PowerShell\u306F\u3001`Test-Path` \u30B3\u30DE\u30F3\
  \u30C9\u30EC\u30C3\u30C8\u3092\u4F7F\u7528\u3057\u3066\u30C7\u30A3\u30EC\u30AF\u30C8\
  \u30EA\u306E\u5B58\u5728\u3092\u30C1\u30A7\u30C3\u30AF\u3059\u308B\u7C21\u5358\u306A\
  \u65B9\u6CD5\u3092\u63D0\u4F9B\u3057\u307E\u3059\u3002\u3053\u306E\u30B3\u30DE\u30F3\
  \u30C9\u30EC\u30C3\u30C8\u306F\u3001\u6307\u5B9A\u3055\u308C\u305F\u30D1\u30B9\u304C\
  \u5B58\u5728\u3059\u308B\u304B\u3069\u3046\u304B\u3092\u793A\u3059\u30D6\u30FC\u30EB\
  \u5024\u3092\u8FD4\u3057\u307E\u3059\u3002\u3053\u3053\u306B\u305D\u306E\u4F7F\u7528\
  \u65B9\u6CD5\u304C\u3042\u308A\u307E\u3059\uFF1A."
lastmod: '2024-03-13T22:44:42.457265-06:00'
model: gpt-4-0125-preview
summary: "PowerShell\u306F\u3001`Test-Path` \u30B3\u30DE\u30F3\u30C9\u30EC\u30C3\u30C8\
  \u3092\u4F7F\u7528\u3057\u3066\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u306E\u5B58\u5728\
  \u3092\u30C1\u30A7\u30C3\u30AF\u3059\u308B\u7C21\u5358\u306A\u65B9\u6CD5\u3092\u63D0\
  \u4F9B\u3057\u307E\u3059\u3002\u3053\u306E\u30B3\u30DE\u30F3\u30C9\u30EC\u30C3\u30C8\
  \u306F\u3001\u6307\u5B9A\u3055\u308C\u305F\u30D1\u30B9\u304C\u5B58\u5728\u3059\u308B\
  \u304B\u3069\u3046\u304B\u3092\u793A\u3059\u30D6\u30FC\u30EB\u5024\u3092\u8FD4\u3057\
  \u307E\u3059\u3002\u3053\u3053\u306B\u305D\u306E\u4F7F\u7528\u65B9\u6CD5\u304C\u3042\
  \u308A\u307E\u3059\uFF1A."
title: "\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u304C\u5B58\u5728\u3059\u308B\u304B\u3069\
  \u3046\u304B\u306E\u78BA\u8A8D"
weight: 20
---

## 方法：
PowerShellは、`Test-Path` コマンドレットを使用してディレクトリの存在をチェックする簡単な方法を提供します。このコマンドレットは、指定されたパスが存在するかどうかを示すブール値を返します。ここにその使用方法があります：

```powershell
# ディレクトリが存在するか確認
$directoryPath = "C:\ExamplePath"
$directoryExists = Test-Path -Path $directoryPath
Write-Output "ディレクトリは存在しますか？ $directoryExists"
```

ディレクトリが存在する場合のサンプル出力：

```
ディレクトリは存在しますか？ True
```

ディレクトリが存在しない場合のサンプル出力：

```
ディレクトリは存在しますか？ False
```

ネットワーク共有やクラウドストレージとの対話を含むより複雑なスクリプトの場合、`Test-Path` が直接提供する機能だけではなく、追加のチェックや機能が必要になる場合があります。そのような場合は、サードパーティのPowerShellモジュールやライブラリを利用することが有益であるかもしれませんが、ほとんどのルーチンタスクはPowerShellの組み込みコマンドレットで達成できます。私の最後の知識更新時点では、`Test-Path` が提供するものを超えてディレクトリの存在を確認するための広く採用されたサードパーティのライブラリは存在していませんでした。主に`Test-Path` 自身がこの目的に対してとても堅牢で効率的だからです。
