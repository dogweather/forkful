---
date: 2024-01-26 03:37:22.479279-07:00
description: "Refactoring ist der Prozess der Umstrukturierung bestehenden Computer-Codes,\
  \ ohne sein externes Verhalten zu \xE4ndern, mit dem Ziel, nichtfunktionale\u2026"
lastmod: 2024-02-19 22:05:13.043776
model: gpt-4-0125-preview
summary: "Refactoring ist der Prozess der Umstrukturierung bestehenden Computer-Codes,\
  \ ohne sein externes Verhalten zu \xE4ndern, mit dem Ziel, nichtfunktionale\u2026"
title: Refactoring
---

{{< edit_this_page >}}

## Was & Warum?
Refactoring ist der Prozess der Umstrukturierung bestehenden Computer-Codes, ohne sein externes Verhalten zu ändern, mit dem Ziel, nichtfunktionale Attribute der Software zu verbessern. Programmierer führen ein Refactoring des Codes durch, um ihn sauberer, effizienter und leichter verständlich zu machen, was die Wartung erleichtert und zukünftige Erweiterungen vereinfacht.

## Wie:
PowerShell bietet kein eigenes, dediziertes Refactoring-Tool, aber Sie können trotzdem Ihren Code bereinigen, um Lesbarkeit und Leistung zu verbessern. Betrachten Sie eine Funktion, die zu viel macht, und wie wir sie für mehr Klarheit refactorn könnten:

```PowerShell
function Get-InventoryData {
    # Ursprüngliche Funktion, die Datenabruf und Formatierung kombiniert
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

# Refaktoriert in separate Funktionen
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

# Verwendung
$inventory = Import-InventoryData -Path 'C:\inventory-list.txt'
Format-InventoryData -Data $inventory
```

Beispielausgabe:

```
ItemID Name            Count Price
------ ----            ----- -----
1001   Widget Typ A    50    9.99
1002   Gadget Typ B    20    14.99
```

## Vertiefung
Refactoring in der Programmierung hat seine Wurzeln, die bis in die frühesten Tage der Softwareentwicklung zurückreichen, obwohl es in den 1990er Jahren als Praxis formalisiert wurde. Martin Fowlers Buch "Refactoring: Improving the Design of Existing Code" ist eines der grundlegenden Werke zum Thema und betont die Bedeutung des Refactorings, um sauberen Code zu erreichen.

Obwohl PowerShell nicht mit spezifischen Refactoring-Tools wie einige Integrierte Entwicklungsumgebungen (IDEs) für andere Sprachen (denken Sie an Eclipse oder Visual Studio) ausgestattet ist, können Sie trotzdem gute Refactoring-Prinzipien manuell praktizieren. Das Wichtigste, an das Sie sich erinnern sollten, ist, dass Refactoring nicht nur darum geht, Code um des Änderns willen zu ändern, sondern um intentionale, verhaltensbewahrende Modifikationen vorzunehmen, die die Struktur und das Design des Codes verbessern.

Alternativen zum manuellen Refactoring in PowerShell umfassen die Verwendung von IDEs, die die Sprache unterstützen, wie Visual Studio Code mit der PowerShell-Erweiterung, die Funktionen wie Codeformatierung und grundlegende Refactoring-Fähigkeiten bietet. Für umfangreicheres Refactoring könnten Sie in Betracht ziehen, Pester-Tests zu nutzen, um sicherzustellen, dass Änderungen die Funktionalität nicht verändern.

Darüber hinaus kann die Implementierung von Refactoring systemischere Änderungen wie Modularisierung umfassen, bei der Code in wiederverwendbare Module oder Funktionen aufgeteilt wird, was die Einhaltung des DRY-Prinzips (Don't Repeat Yourself) verbessert. Andere übliche Refactoring-Techniken umfassen die Umbenennung zur Klärung, das Entfernen von doppeltem Code und die Reduzierung der Komplexität von bedingter Logik.

## Siehe auch
Für eine tiefergehende Betrachtung finden Sie hier einige Ressourcen:

- Martin Fowlers Buch zum Refactoring: [_Refactoring: Improving the Design of Existing Code_](https://martinfowler.com/books/refactoring.html)
- Testen von refaktoriertem Code mit Pester: [Pester Test Framework](https://pester.dev/)
- PowerShell Best Practices: [Der PowerShell Best Practices und Style Guide](https://poshcode.gitbooks.io/powershell-practice-and-style/)
