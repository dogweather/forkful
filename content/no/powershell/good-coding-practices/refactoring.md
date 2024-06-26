---
date: 2024-01-26 03:36:56.802495-07:00
description: "Hvordan: PowerShell har ikke et dedikert refaktoreringsverkt\xF8y innebygd,\
  \ men du kan fortsatt rydde opp i koden din for lesbarhet og ytelse. Vurder en\u2026"
lastmod: '2024-03-13T22:44:41.025625-06:00'
model: gpt-4-0125-preview
summary: "PowerShell har ikke et dedikert refaktoreringsverkt\xF8y innebygd, men du\
  \ kan fortsatt rydde opp i koden din for lesbarhet og ytelse."
title: Refaktorering
weight: 19
---

## Hvordan:
PowerShell har ikke et dedikert refaktoreringsverktøy innebygd, men du kan fortsatt rydde opp i koden din for lesbarhet og ytelse. Vurder en funksjon som gjør for mye og hvordan vi kan refaktorere den for klarhet:

```PowerShell
function Get-InventoryData {
    # Original funksjon som kombinerer datahenting og formatering
    $data = Get-Content -Path 'C:\inventory-list.txt'
    $inventoryData = $data | ForEach-Object {
        $fields = $_ -split ','
        [PSCustomObject]@{
            ItemID = $fields[0]
            Navn   = $fields[1]
            Antall = $fields[2]
            Pris   = $fields[3]
        }
    }
    $inventoryData | Format-Table -AutoSize
}

# Refaktorert til separate funksjoner
function Import-InventoryData {
    param($Path)
    Get-Content -Path $Path | ForEach-Object {
        $fields = $_ -split ','
        [PSCustomObject]@{
            ItemID = $fields[0]
            Navn   = $fields[1]
            Antall = $fields[2]
            Pris   = $fields[3]
        }
    }
}

function Format-InventoryData {
    param($Data)
    $Data | Format-Table -AutoSize
}

# Bruk
$inventory = Import-InventoryData -Path 'C:\inventory-list.txt'
Format-InventoryData -Data $inventory
```

Eksempel på utdata:

```
ItemID Navn            Antall Pris
------ ----            ------ -----
1001   Widget Type A   50     9.99
1002   Gadget Type B   20     14.99
```

## Dypdykk
Refaktorering i programmering har røtter som strekker seg tilbake til de tidligste dager av programvareutvikling, selv om det ble formalisert som en praksis på 1990-tallet. Martin Fowlers bok "Refactoring: Improving the Design of Existing Code" er en av de grunnleggende verkene om emnet, og understreker viktigheten av refaktorering for å oppnå ren kode.

Selv om PowerShell ikke kommer med spesifikke refaktoreringsverktøy som noen integrerte utviklingsmiljøer (IDEer) for andre språk gjør (tenk Eclipse eller Visual Studio), kan du fortsatt praktisere gode refaktoreringsprinsipper manuelt. Det viktigste å huske på er at refaktorering ikke bare handler om å endre kode for endringens skyld, men å gjøre bevisste, atferdsbevarende modifikasjoner som forbedrer kodens struktur og design.

Alternativer til manuell refaktorering i PowerShell inkluderer bruk av IDEer som støtter språket, som Visual Studio Code med PowerShell-utvidelsen, noe som tilbyr funksjoner som kodeformatering og grunnleggende refaktoreringsmuligheter. For mer betydelig refaktorering, kan du vurdere å bruke Pester-tester for å sikre at endringene ikke endrer funksjonaliteten.

I tillegg kan implementering av refaktorering innebære mer systemiske endringer som modularisering, hvor kode deles inn i gjenbrukbare moduler eller funksjoner, som forbedrer etterlevelsen av DRY (Don't Repeat Yourself)-prinsippet. Andre vanlige refaktoreringsteknikker inkluderer omdøping for klarhet, fjerning av duplisert kode og reduksjon av kompleksiteten i betinget logikk.

## Se også
For å dykke dypere, her er noen ressurser:

- Martin Fowlers Refaktoreringsbok: [_Refaktorering: Forbedring av designet på eksisterende kode_](https://martinfowler.com/books/refactoring.html)
- Testing av refaktorert kode med Pester: [Pester Testing Framework](https://pester.dev/)
- Beste praksiser i PowerShell: [PowerShell Beste Praksiser og Stilguide](https://poshcode.gitbooks.io/powershell-practice-and-style/)
