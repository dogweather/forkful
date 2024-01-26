---
title:                "Att organisera kod i funktioner"
date:                  2024-01-26T01:11:44.591230-07:00
model:                 gpt-4-1106-preview
simple_title:         "Att organisera kod i funktioner"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att organisera kod i funktioner handlar om att kapsla in delar av kod som utför specifika uppgifter och ge dem ett namn. Det görs för att göra koden återanvändbar, läslig och underhållbar. Istället för att omskriva samma kod, anropa en funktion. Vill du felsöka eller uppgradera? Justera funktionen utan att behöva gräva genom högar av skript.

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