---
title:                "重构"
aliases: - /zh/powershell/refactoring.md
date:                  2024-01-26T03:37:40.826127-07:00
model:                 gpt-4-0125-preview
simple_title:         "重构"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/powershell/refactoring.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
重构是在不改变其外部行为的情况下重新结构现有计算机代码的过程，目的是改善软件的非功能属性。程序员重构代码是为了让代码更清晰、更高效、更易于理解，从而便于更容易地维护和未来的增强。

## 如何操作：
PowerShell内置的并没有专门的重构工具，但你仍然可以清理你的代码，以提高可读性和性能。考虑一个做得太多的函数，以及我们如何为了清晰而重构它：

```PowerShell
function Get-InventoryData {
    # 原始函数结合了数据检索和格式化
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

# 重构为独立的函数
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

# 使用案例
$inventory = Import-InventoryData -Path 'C:\inventory-list.txt'
Format-InventoryData -Data $inventory
```

样本输出：

```
ItemID Name            Count Price
------ ----            ----- -----
1001   Widget Type A   50    9.99
1002   Gadget Type B   20    14.99
```

## 深入探索
编程中的重构根源可以追溯到软件开发的最初日子，尽管到了1990年代，重构作为一种实践才被正式化。马丁·福勒的《重构：改善既有代码的设计》是该主题上的开创性著作之一，强调了重构在实现清晰代码中的重要性。

尽管PowerShell没有像其他语言的一些集成开发环境（IDE）那样的具体重构工具（比如Eclipse或Visual Studio），但你仍然可以手动实践良好的重构原则。需要记住的关键一点是，重构不仅仅是为了改变代码而改变代码，而是做出有意的、保持行为的修改，以增强代码的结构和设计。

PowerShell手动重构的替代方式包括使用支持该语言的IDE，如带有PowerShell扩展的Visual Studio Code，它提供了代码格式化和基本重构能力等特性。对于更重要的重构，你可能考虑利用Pester测试来确保更改不会改变功能。

此外，重构的实现可以涉及更多系统性的变化，如模块化，其中代码被分割成可重用的模块或函数，改进了DRY（不要自己重复）原则的遵守。其他常见的重构技术包括为了清晰而重命名、移除重复代码，以及减少条件逻辑的复杂性。

## 另请参见
要深入了解，这里有一些资源：

- 马丁·福勒的重构书：[《重构：改善既有代码的设计》](https://martinfowler.com/books/refactoring.html)
- 使用Pester测试重构后的代码：[Pester测试框架](https://pester.dev/)
- PowerShell最佳实践：[PowerShell最佳实践和风格指南](https://poshcode.gitbooks.io/powershell-practice-and-style/)
