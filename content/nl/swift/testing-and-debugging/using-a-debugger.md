---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:10.174448-07:00
description: "Een debugger gebruiken betekent het inzetten van gespecialiseerde tools\
  \ om je code te testen en te inspecteren terwijl deze draait. Het is een belangrijke\u2026"
lastmod: '2024-02-25T18:49:48.492410-07:00'
model: gpt-4-0125-preview
summary: "Een debugger gebruiken betekent het inzetten van gespecialiseerde tools\
  \ om je code te testen en te inspecteren terwijl deze draait. Het is een belangrijke\u2026"
title: Een debugger gebruiken
---

{{< edit_this_page >}}

## Wat & Waarom?
Een debugger gebruiken betekent het inzetten van gespecialiseerde tools om je code te testen en te inspecteren terwijl deze draait. Het is een belangrijke zaak omdat het je laat zien wat er onder de motorkap gebeurt, bugs vinden, en je codegedrag beter begrijpen.

## Hoe te:
Om de debugger in Xcode (de IDE voor Swift) te gebruiken, kun je breakpoints instellen, variabelen inspecteren en expressies volgen. Hier is een voorbeeld:

```Swift
func vindFactorial(van nummer: Int) -> Int {
    if nummer == 0 {
        return 1
    }
    return nummer * vindFactorial(van: nummer - 1)
}

let resultaat = vindFactorial(van: 5)
print(resultaat)
```

Stel een breakpoint in door links van een regelnummer in Xcode te klikken, en draai het programma. Wanneer het bij de breakpoint komt, pauzeert Xcode de uitvoering. Nu kun je:

1. De waarden van variabelen controleren.
2. Overstappen (de volgende regel uitvoeren) of instappen (binnen een functie gaan) met de debugger-bedieningselementen.
3. Expressies toevoegen aan de 'watch list' om veranderingen in specifieke variabelen of constanten te monitoren.

Hier is wat je misschien ziet in het debuggebied:

```
(lldb) po nummer
5
(lldb) po resultaat
120
```

## Diepe Duik:
Debuggers zijn deel van het programmeerlandschap sinds de jaren 1940, geëvolueerd van simpele breakpointsystemen naar complexe, UI-gestuurde ervaringen. Andere opties naast de ingebouwde debugger van Xcode omvatten tools van derden zoals LLDB (Low Level Debugger) die Xcode onder de motorkap gebruikt. Sommige mensen debuggen zelfs met `print()` statements (liefkozend "caveman debugging" genoemd), maar dit is minder efficiënt voor grote projecten of complexe bugs. Wanneer je een debugger gebruikt, jongleer je met uitvoeringscontrole, runtime-introspectie en datamanipulatie. Een diep begrip van deze principes gaat een lange weg in efficiënt debuggen.

## Zie Ook:
- [Xcode Debugginggids van Apple](https://developer.apple.com/documentation/xcode/debugging/)
- [LLDB Quick Start Gids](https://lldb.llvm.org/use/tutorial.html)
- [Ray Wenderlich's Swift Debuggen Tutoriaal](https://www.raywenderlich.com/966538-arc-and-memory-management-in-swift)
