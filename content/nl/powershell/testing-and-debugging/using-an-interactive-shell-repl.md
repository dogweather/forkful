---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:05.383777-07:00
description: "De interactieve shell, of Lees-Evalueer-Print Lus (REPL), stelt je in\
  \ staat om PowerShell-commando's te typen en onmiddellijke feedback te krijgen.\u2026"
lastmod: '2024-02-25T18:49:48.366386-07:00'
model: gpt-4-0125-preview
summary: "De interactieve shell, of Lees-Evalueer-Print Lus (REPL), stelt je in staat\
  \ om PowerShell-commando's te typen en onmiddellijke feedback te krijgen.\u2026"
title: Het gebruik van een interactieve shell (REPL)
---

{{< edit_this_page >}}

## Wat & Waarom?
De interactieve shell, of Lees-Evalueer-Print Lus (REPL), stelt je in staat om PowerShell-commando's te typen en onmiddellijke feedback te krijgen. Programmeurs gebruiken het om snel codefragmenten te testen, te debuggen of nieuwe commando's te leren zonder een volledig script te schrijven.

## Hoe:
Start PowerShell en je bevindt je in de REPL. Probeer de `Get-Date` Cmdlet:

```PowerShell
PS > Get-Date
```

Je zou de huidige datum en tijd als uitvoer moeten zien:

```PowerShell
Woensdag, 31 maart 2023 12:34:56 PM
```

Nu, keten commando's. Laten we processen sorteren op geheugengebruik:

```PowerShell
PS > Get-Process | Sort-Object WS -Aflopend | Select-Object -First 5
```

Dit geeft de top 5 processen uit op basis van de grootte van de werkset (geheugengebruik).

## Diepgaande duik
PowerShell's REPL heeft zijn wortels in de Unix shell en andere dynamische taalshells zoals die van Python. Het is een enkelgebruiker, interactieve commando-uitvoeringsomgeving. In tegenstelling tot een gecompileerde taal waar je hele applicaties schrijft en vervolgens compileert, laat een REPL-omgeving je toe om code regel voor regel te schrijven en uit te voeren. PowerShell ondersteunt ook de uitvoering van scripts voor grotere taken.

Alternatieven voor Windows zijn de Opdrachtprompt of andere taalspecifieke REPL's zoals IPython. In de Unix/Linux-wereld vervullen shells zoals bash of zsh een soortgelijke functie.

De implementatie van PowerShell maakt gebruik van een hosttoepassing om de shell te draaien. Hoewel PowerShell.exe in Windows het meest voorkomende is, kunnen anderen zoals de Integrated Scripting Environment (ISE) of de geïntegreerde terminal van Visual Studio Code ook als de host dienen.

## Zie Ook
- [Over PowerShell](https://docs.microsoft.com/nl-nl/powershell/scripting/overview)
- [StackOverflow: PowerShell](https://stackoverflow.com/questions/tagged/powershell)
