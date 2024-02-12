---
title:                "Afronden van getallen"
aliases:
- /nl/powershell/rounding-numbers.md
date:                  2024-01-28T22:06:49.503462-07:00
model:                 gpt-4-0125-preview
simple_title:         "Afronden van getallen"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/powershell/rounding-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Afronden van getallen gaat over het aanpassen van een waarde naar het dichtstbijzijnde geheel getal of opgegeven decimale plaats. Programmeurs ronden getallen af om gegevens te vereenvoudigen, leesbaarheid te verbeteren of aan bepaalde wiskundige eisen te voldoen tijdens berekeningen.

## Hoe:
Je hebt een paar handige cmdlets en methoden in PowerShell voor het afronden:

- `Round()` methode uit de Math-klasse
```PowerShell
[Math]::Round(15.68) # Rondt af naar 16
```
- Decimalen specificeren:
```PowerShell
[Math]::Round(15.684, 2) # Rondt af naar 15.68
```
- `Ceiling()` en `Floor()`, voor altijd naar boven of naar beneden afronden:
```PowerShell
[Math]::Ceiling(15.2) # Rondt naar boven af naar 16
[Math]::Floor(15.9) # Rondt naar beneden af naar 15
```

## Diepere Duik
Afronden van getallen is geen nieuwkomer; het bestaat al sinds de oudheid, nuttig voor handel, wetenschap en tijdmeting. Als het gaat om PowerShell, volgt `[Math]::Round()` standaard de "Bankiers Afronding", waarbij 0,5 naar het dichtstbijzijnde even getal gaat, wat vooringenomenheid in statistische operaties vermindert.

Je bent niet alleen beperkt tot `[Math]` methoden. Wil je meer controle? Bekijk `[System.Math]::Round(Getal, Aantalcijfers, MidpointRounding)` waar je kunt instellen hoe tussenpunten worden afgehandeld: van nul af of naar even (ook wel Bankiers Afronding genoemd).

Een andere invalshoek: het `System.Globalization.CultureInfo` object. Het helpt bij het opmaakspecifiek en afrondvoorkeuren behandelen bij het omgaan met internationale getallen.

## Zie Ook
- Microsoft's officiële documentatie over Math-methoden: [Link](https://learn.microsoft.com/nl-nl/dotnet/api/system.math?view=net-7.0)
- Details over decimale afronding in .NET: [Link](https://learn.microsoft.com/nl-nl/dotnet/api/system.midpointrounding?view=net-7.0)
- Discussies over afronden op StackOverflow: [Link](https://stackoverflow.com/questions/tagged/rounding+powershell)
