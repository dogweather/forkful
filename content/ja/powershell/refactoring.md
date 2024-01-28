---
title:                "リファクタリング"
date:                  2024-01-26T03:37:30.491811-07:00
model:                 gpt-4-0125-preview
simple_title:         "リファクタリング"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/powershell/refactoring.md"
---

{{< edit_this_page >}}

## 何となぜ？
リファクタリングとは、既存のコンピュータコードの構造を変更するプロセスであり、外部の振る舞いを変更せずにソフトウェアの非機能的属性を改善することを目的としています。プログラマーは、コードをよりクリーンで、効率的で、理解しやすくするためにリファクタリングを行い、これによりメンテナンスが容易になり将来の拡張が容易になります。

## 方法
PowerShellには専用のリファクタリングツールは組み込まれていませんが、読みやすさやパフォーマンスのためにコードを整理することはできます。多くの処理を行っている関数を考え、それを明確にリファクタリングする方法を検討してみましょう：

```PowerShell
function Get-InventoryData {
    # データ取得とフォーマットを組み合わせた元の関数
    $data = Get-Content -Path 'C:\inventory-list.txt'
    $inventoryData = $data | ForEach-Object {
        $fields = $_ -split ','
        [PSCustomObject]@{
            ItemID = $fields[0]
            Name   = $fields[1]
            Count  = $fields[2]
            Price  = $fields[3]
        }
    }
    $inventoryData | Format-Table -AutoSize
}

# 関数を分割してリファクタリング
function Import-InventoryData {
    param($Path)
    Get-Content -Path $Path | ForEach-Object {
        $fields = $_ -split ','
        [PSCustomObject]@{
            ItemID = $fields[0]
            Name   = $fields[1]
            Count  = $fields[2]
            Price  = $fields[3]
        }
    }
}

function Format-InventoryData {
    param($Data)
    $Data | Format-Table -AutoSize
}

# 使用例
$inventory = Import-InventoryData -Path 'C:\inventory-list.txt'
Format-InventoryData -Data $inventory
```

サンプル出力：

```
ItemID Name            Count Price
------ ----            ----- -----
1001   Widget Type A   50    9.99
1002   Gadget Type B   20    14.99
```

## 掘り下げ
プログラミングにおけるリファクタリングは、ソフトウェア開発の最初の日から根付いていますが、1990年代に一般的な実践として形式化されました。マーチン・ファウラーの著書「Refactoring: Improving the Design of Existing Code」は、クリーンなコードを達成するためにリファクタリングの重要性を強調した基本的な作品の一つです。

PowerShellには、他の言語のいくつかの統合開発環境（IDE）（EclipseやVisual Studioなど）のような特定のリファクタリングツールが付属していませんが、手動で良いリファクタリングの原則を実践することはできます。リファクタリングは単にコードを変更することではなく、コードの構造とデザインを向上させるための意図的で、振る舞いを保持する変更を行うことであることを覚えておくことが重要です。

PowerShellでの手動リファクタリングに代わるものには、言語をサポートするIDE（例えば、PowerShell拡張機能を備えたVisual Studio Code）を使用することが含まれます。これは、コードのフォーマットや基本的なリファクタリング機能のような特徴を提供します。より大規模なリファクタリングには、変更が機能に影響を与えないことを確認するためにPesterテストを活用することを検討するかもしれません。

また、リファクタリングの実装には、コードを再利用可能なモジュールまたは関数に分割するような、よりシステム的な変更が含まれることがあります。これはDRY（Don't Repeat Yourself）の原則の遵守を改善します。その他の一般的なリファクタリングの技法には、明確性のための名前変更、重複コードの削除、条件ロジックの複雑さの削減が含まれます。

## 参照
さらに深く掘り下げるために、以下のリソースをご覧ください：

- マーチン・ファウラーのリファクタリングの本：[_Refactoring: Improving the Design of Existing Code_](https://martinfowler.com/books/refactoring.html)
- リファクタリングされたコードのテストをPesterで行う：[Pester Testing Framework](https://pester.dev/)
- PowerShellのベストプラクティス：[The PowerShell Best Practices and Style Guide](https://poshcode.gitbooks.io/powershell-practice-and-style/)
