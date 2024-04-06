---
date: 2024-01-26 03:37:30.491811-07:00
description: "\u65B9\u6CD5 PowerShell\u306B\u306F\u5C02\u7528\u306E\u30EA\u30D5\u30A1\
  \u30AF\u30BF\u30EA\u30F3\u30B0\u30C4\u30FC\u30EB\u306F\u7D44\u307F\u8FBC\u307E\u308C\
  \u3066\u3044\u307E\u305B\u3093\u304C\u3001\u8AAD\u307F\u3084\u3059\u3055\u3084\u30D1\
  \u30D5\u30A9\u30FC\u30DE\u30F3\u30B9\u306E\u305F\u3081\u306B\u30B3\u30FC\u30C9\u3092\
  \u6574\u7406\u3059\u308B\u3053\u3068\u306F\u3067\u304D\u307E\u3059\u3002\u591A\u304F\
  \u306E\u51E6\u7406\u3092\u884C\u3063\u3066\u3044\u308B\u95A2\u6570\u3092\u8003\u3048\
  \u3001\u305D\u308C\u3092\u660E\u78BA\u306B\u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\
  \u30B0\u3059\u308B\u65B9\u6CD5\u3092\u691C\u8A0E\u3057\u3066\u307F\u307E\u3057\u3087\
  \u3046\uFF1A."
lastmod: '2024-04-05T22:38:41.955282-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5 PowerShell\u306B\u306F\u5C02\u7528\u306E\u30EA\u30D5\u30A1\u30AF\
  \u30BF\u30EA\u30F3\u30B0\u30C4\u30FC\u30EB\u306F\u7D44\u307F\u8FBC\u307E\u308C\u3066\
  \u3044\u307E\u305B\u3093\u304C\u3001\u8AAD\u307F\u3084\u3059\u3055\u3084\u30D1\u30D5\
  \u30A9\u30FC\u30DE\u30F3\u30B9\u306E\u305F\u3081\u306B\u30B3\u30FC\u30C9\u3092\u6574\
  \u7406\u3059\u308B\u3053\u3068\u306F\u3067\u304D\u307E\u3059\u3002\u591A\u304F\u306E\
  \u51E6\u7406\u3092\u884C\u3063\u3066\u3044\u308B\u95A2\u6570\u3092\u8003\u3048\u3001\
  \u305D\u308C\u3092\u660E\u78BA\u306B\u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\u30B0\
  \u3059\u308B\u65B9\u6CD5\u3092\u691C\u8A0E\u3057\u3066\u307F\u307E\u3057\u3087\u3046\
  \uFF1A."
title: "\u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\u30B0"
weight: 19
---

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
