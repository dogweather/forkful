---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:45.867634-07:00
description: "\u65B9\u6CD5\uFF1A PowerShell\u306B\u306F\u7D44\u307F\u8FBC\u307F\u306E\
  \u30C6\u30B9\u30C8\u30D5\u30EC\u30FC\u30E0\u30EF\u30FC\u30AF\u304C\u3042\u308A\u307E\
  \u305B\u3093\u304C\u3001Pester\uFF08\u4EBA\u6C17\u306E\u3042\u308B\u30B5\u30FC\u30C9\
  \u30D1\u30FC\u30C6\u30A3\u30E2\u30B8\u30E5\u30FC\u30EB\uFF09\u304C\u30C6\u30B9\u30C8\
  \u306E\u4F5C\u6210\u3068\u5B9F\u884C\u306B\u5E83\u304F\u4F7F\u7528\u3055\u308C\u3066\
  \u3044\u307E\u3059\u3002PowerShell\u95A2\u6570\u306E\u30C6\u30B9\u30C8\u306BPester\u3092\
  \u4F7F\u3044\u59CB\u3081\u308B\u65B9\u6CD5\u306F\u6B21\u306E\u901A\u308A\u3067\u3059\
  \u3002\u2026"
lastmod: '2024-03-13T22:44:42.441976-06:00'
model: gpt-4-0125-preview
summary: "PowerShell\u306B\u306F\u7D44\u307F\u8FBC\u307F\u306E\u30C6\u30B9\u30C8\u30D5\
  \u30EC\u30FC\u30E0\u30EF\u30FC\u30AF\u304C\u3042\u308A\u307E\u305B\u3093\u304C\u3001\
  Pester\uFF08\u4EBA\u6C17\u306E\u3042\u308B\u30B5\u30FC\u30C9\u30D1\u30FC\u30C6\u30A3\
  \u30E2\u30B8\u30E5\u30FC\u30EB\uFF09\u304C\u30C6\u30B9\u30C8\u306E\u4F5C\u6210\u3068\
  \u5B9F\u884C\u306B\u5E83\u304F\u4F7F\u7528\u3055\u308C\u3066\u3044\u307E\u3059\u3002\
  PowerShell\u95A2\u6570\u306E\u30C6\u30B9\u30C8\u306BPester\u3092\u4F7F\u3044\u59CB\
  \u3081\u308B\u65B9\u6CD5\u306F\u6B21\u306E\u901A\u308A\u3067\u3059."
title: "\u30C6\u30B9\u30C8\u306E\u4F5C\u6210"
weight: 36
---

## 方法：
PowerShellには組み込みのテストフレームワークがありませんが、Pester（人気のあるサードパーティモジュール）がテストの作成と実行に広く使用されています。PowerShell関数のテストにPesterを使い始める方法は次の通りです。

まず、まだPesterをインストールしていない場合は、インストールします：

```powershell
Install-Module -Name Pester -Scope CurrentUser -Force
```

次に、テストしたい単純なPowerShell関数が`MyFunction.ps1`として保存されているとします：

```powershell
function Get-MultipliedNumber {
    param (
        [int]$Number,
        [int]$Multiplier = 2
    )

    return $Number * $Multiplier
}
```

この関数をPesterでテストするには、`MyFunction.Tests.ps1`という名前のテストスクリプトを作成します。このスクリプトでは、Pesterの`Describe`と`It`ブロックを使ってテストケースを定義します：

```powershell
# テストする関数をインポートする
. .\MyFunction.ps1

Describe "Get-MultipliedNumber tests" {
    It "倍率を提供しない場合、数値に2を乗算する" {
        $result = Get-MultipliedNumber -Number 3
        $result | Should -Be 6
    }

    It "指定された倍率で数値を正しく乗算する" {
        $result = Get-MultipliedNumber -Number 3 -Multiplier 3
        $result | Should -Be 9
    }
}
```

テストを実行するには、PowerShellを開き、テストスクリプトが含まれるディレクトリに移動し、`Invoke-Pester`コマンドを使用します：

```powershell
Invoke-Pester .\MyFunction.Tests.ps1
```

出力サンプルは次のようになり、テストが合格したか失敗したかを示します：

```
Starting discovery in 1 files.
Discovery finished in 152ms.
[+] C:\path\to\MyFunction.Tests.ps1 204ms (182ms|16ms)
Tests completed in 204ms
Tests Passed: 2, Failed: 0, Skipped: 0 NotRun: 0
```

この出力は、テストされたシナリオの下で`Get-MultipliedNumber`関数が期待通りに動作するという自信を与えてくれる、2つのテストが合格したことを示しています。
