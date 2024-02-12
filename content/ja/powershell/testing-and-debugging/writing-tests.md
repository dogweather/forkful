---
title:                "テストの作成"
aliases:
- ja/powershell/writing-tests.md
date:                  2024-02-03T19:31:45.867634-07:00
model:                 gpt-4-0125-preview
simple_title:         "テストの作成"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
