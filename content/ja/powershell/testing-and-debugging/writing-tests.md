---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:45.867634-07:00
description: "PowerShell\u3067\u30C6\u30B9\u30C8\u3092\u66F8\u304F\u3068\u306F\u3001\
  PowerShell\u30B3\u30FC\u30C9\u306E\u6A5F\u80FD\u3092\u81EA\u52D5\u7684\u306B\u691C\
  \u8A3C\u3059\u308B\u30B9\u30AF\u30EA\u30D7\u30C8\u3092\u4F5C\u6210\u3059\u308B\u3053\
  \u3068\u3067\u3001\u671F\u5F85\u901A\u308A\u306B\u52D5\u4F5C\u3059\u308B\u3053\u3068\
  \u3092\u78BA\u8A8D\u3059\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u30FC\u306F\u3053\u308C\u3092\u884C\u3046\u3053\u3068\u3067\u3001\u30D0\u30B0\
  \u3092\u65E9\u671F\u306B\u767A\u898B\u3057\u3001\u30B3\u30FC\u30C9\u306E\u30E1\u30F3\
  \u30C6\u30CA\u30F3\u30B9\u3092\u7C21\u7D20\u5316\u3057\u3001\u30B3\u30FC\u30C9\u306E\
  \u5909\u66F4\u304C\u65E2\u5B58\u306E\u6A5F\u80FD\u3092\u8AA4\u3063\u3066\u58CA\u3055\
  \u306A\u3044\u3088\u3046\u306B\u3057\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:42.441976-06:00'
model: gpt-4-0125-preview
summary: "PowerShell\u3067\u30C6\u30B9\u30C8\u3092\u66F8\u304F\u3068\u306F\u3001PowerShell\u30B3\
  \u30FC\u30C9\u306E\u6A5F\u80FD\u3092\u81EA\u52D5\u7684\u306B\u691C\u8A3C\u3059\u308B\
  \u30B9\u30AF\u30EA\u30D7\u30C8\u3092\u4F5C\u6210\u3059\u308B\u3053\u3068\u3067\u3001\
  \u671F\u5F85\u901A\u308A\u306B\u52D5\u4F5C\u3059\u308B\u3053\u3068\u3092\u78BA\u8A8D\
  \u3059\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\
  \u3053\u308C\u3092\u884C\u3046\u3053\u3068\u3067\u3001\u30D0\u30B0\u3092\u65E9\u671F\
  \u306B\u767A\u898B\u3057\u3001\u30B3\u30FC\u30C9\u306E\u30E1\u30F3\u30C6\u30CA\u30F3\
  \u30B9\u3092\u7C21\u7D20\u5316\u3057\u3001\u30B3\u30FC\u30C9\u306E\u5909\u66F4\u304C\
  \u65E2\u5B58\u306E\u6A5F\u80FD\u3092\u8AA4\u3063\u3066\u58CA\u3055\u306A\u3044\u3088\
  \u3046\u306B\u3057\u307E\u3059\u3002"
title: "\u30C6\u30B9\u30C8\u306E\u4F5C\u6210"
---

{{< edit_this_page >}}

## 何となぜ？

PowerShellでテストを書くとは、PowerShellコードの機能を自動的に検証するスクリプトを作成することで、期待通りに動作することを確認することです。プログラマーはこれを行うことで、バグを早期に発見し、コードのメンテナンスを簡素化し、コードの変更が既存の機能を誤って壊さないようにします。

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
