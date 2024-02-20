---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:26.716334-07:00
description: "Het schrijven van tests in programmeren betekent het maken van scripts\
  \ die controleren of je code correct werkt. Programmeurs doen dit om bugs vroeg\
  \ op te\u2026"
lastmod: 2024-02-19 22:05:10.117129
model: gpt-4-0125-preview
summary: "Het schrijven van tests in programmeren betekent het maken van scripts die\
  \ controleren of je code correct werkt. Programmeurs doen dit om bugs vroeg op te\u2026"
title: Tests Schrijven
---

{{< edit_this_page >}}

## Wat & Waarom?
Het schrijven van tests in programmeren betekent het maken van scripts die controleren of je code correct werkt. Programmeurs doen dit om bugs vroeg op te sporen, betrouwbaarheid te garanderen en te voorkomen dat toekomstige veranderingen bestaande functies breken.

## Hoe te:
Hier is een snelle test voor een functie die getallen optelt met Pester, het testraamwerk van PowerShell. Je zou dit script normaal gesproken opslaan als `Add.Tests.ps1`.

```PowerShell
# Voorbeeldfunctie om te testen
function Add ($a, $b) {
    return $a + $b
}

# Pester module importeren
Import-Module Pester

# De test definiëren
Describe "Add-Functie" {
    It "voegt twee getallen samen" {
        # Arrange
        $num1 = 10
        $num2 = 20
        $expected = 30

        # Act
        $result = Add $num1 $num2

        # Assert
        $result | Should -Be $expected
    }
}

# De test uitvoeren
Invoke-Pester
```

Na het uitvoeren van het script, zie je output zoals:

```
Describing Add-Functie
    [+] voegt twee getallen samen 75ms
Tests voltooid in 75ms
Tests Geslaagd: 1, Mislukt: 0, Overgeslagen: 0 NietUitgevoerd: 0
```

## Diepgaand:
Historisch gezien was het testen in PowerShell veel handmatiger voordat Pester werd geïntroduceerd. Het veranderde het spel door een krachtige, maar simpele syntax te bieden voor geautomatiseerd testen, met concepten geleend van testraamwerken in andere talen. Alternatieven voor Pester omvatten PSUnit en PSTest, maar Pester is het meest gebruikt en direct geïntegreerd in PowerShell Core voor ondersteuning op meerdere platforms. Gedetailleerde implementatie van tests omvat typisch een cyclus die "Rood, Groen, Hervormen" wordt genoemd, waarbij eerst tests worden geschreven om te falen (Rood), vervolgens wordt code geschreven om de tests te laten slagen (Groen), gevolgd door een opruimfase zonder gedrag te veranderen (Hervormen).

## Zie ook:
- Pester's GitHub-repository: [https://github.com/pester/Pester](https://github.com/pester/Pester)
