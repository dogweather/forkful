---
date: 2024-01-26 01:11:44.591230-07:00
description: "Hur man g\xF6r: L\xE5t oss skriva en funktion f\xF6r att ber\xE4kna\
  \ summan av tv\xE5 tal. Enkelt, men det illustrerar po\xE4ngen."
lastmod: '2024-03-13T22:44:38.131349-06:00'
model: gpt-4-1106-preview
summary: "L\xE5t oss skriva en funktion f\xF6r att ber\xE4kna summan av tv\xE5 tal."
title: Att organisera kod i funktioner
weight: 18
---

## Hur man gör:
Låt oss skriva en funktion för att beräkna summan av två tal. Enkelt, men det illustrerar poängen.

```PowerShell
function LäggTill-Tal {
    param (
        [int]$FörstaNum,
        [int]$AndraNum
    )
    return $FörstaNum + $AndraNum
}

# Anropa funktionen med 5 och 10
$summa = LäggTill-Tal -FörstaNum 5 -AndraNum 10
Write-Output "Summan är $summa"
```

Exempel på utdata:

```
Summan är 15
```

## Fördjupning
Funktioner i PowerShell, liksom i de flesta språk, är inga nyheter. Vi har kompartmentaliserat kod sedan Fortran-tiden. Det handlar om 'att inte uppfinna hjulet på nytt'. Alternativ? Självklart, skript eller cmdlets. Men de saknar funktionernas prydhet och kontextkänslighet inom skript.

Implementering? Funktioner kan vara grundläggande som vårt exempel eller komplexa med omfång, pipelineinmatning och mer. Ta `Avancerade Funktioner`. De efterliknar cmdlets med parametrar som har attribut, som `[Parameter(Mandatory=$true)]`. Det är ett smakprov på PowerShell-flexibilitet.

## Se även
- [about_Functions_Advanced_Parameters](https://docs.microsoft.com/sv-se/powershell/module/microsoft.powershell.core/about/about_functions_advanced_parameters?view=powershell-7.1)
- [about_Script_Blocks](https://docs.microsoft.com/sv-se/powershell/module/microsoft.powershell.core/about/about_script_blocks?view=powershell-7.1)
