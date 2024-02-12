---
title:                "Numerojen pyöristäminen"
aliases:
- /fi/powershell/rounding-numbers.md
date:                  2024-01-26T03:46:20.050592-07:00
model:                 gpt-4-0125-preview
simple_title:         "Numerojen pyöristäminen"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/rounding-numbers.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Numeroiden pyöristäminen tarkoittaa arvon säätämistä lähimpään kokonaislukuun tai määriteltyyn desimaalipaikkaan. Ohjelmoijat pyöristävät numeroita yksinkertaistaakseen dataa, parantaakseen luettavuutta tai täyttääkseen tiettyjä matemaattisia vaatimuksia laskentojen aikana.

## Kuinka:
PowerShellissa on muutama kätevä cmdlet ja menetelmä numeroiden pyöristämiseen:

- `Round()` -metodi Math-luokasta
```PowerShell
[Math]::Round(15.68) # Pyöristää 16:een
```
- Määritä desimaalit:
```PowerShell
[Math]::Round(15.684, 2) # Pyöristää 15.68:aan
```
- `Ceiling()` ja `Floor()`, aina pyöristämiseen ylös tai alas:
```PowerShell
[Math]::Ceiling(15.2) # Pyöristää ylöspäin 16:een
[Math]::Floor(15.9) # Pyöristää alaspäin 15:een
```

## Syväsukellus
Numeroiden pyöristäminen ei ole uusi keksintö; se on ollut olemassa muinaisista ajoista lähtien, hyödyllisenä kaupankäynnissä, tieteessä ja ajanlaskussa. PowerShellistä puhuttaessa, `[Math]::Round()` noudattaa oletuksena "Pankkiirin Pyöristystä", jossa 0,5 mennään lähimpään parilliseen numeroon, vähentäen vinoumaa tilastollisissa toiminnoissa.

Et ole juuttunut vain `[Math]` -metodeihin. Haluatko enemmän kontrollia? Tutustu `[System.Math]::Round(Number, Digits, MidpointRounding)` -metodiin, jossa voit säätää, kuinka keskipisteitä käsitellään: poispäin nollasta tai parilliseen (tunnetaan myös Pankkiirin Pyöristyksenä).

Toinen näkökulma: `System.Globalization.CultureInfo` -objekti. Se auttaa kohdekielikohtaisessa muotoilussa ja pyöristysasetuksissa kun käsitellään kansainvälisiä numeroita.

## Katso Myös
- Microsoftin viralliset dokumetit Math-metodeista: [Linkki](https://learn.microsoft.com/en-us/dotnet/api/system.math?view=net-7.0)
- Desimaalien pyöristämisen yksityiskohdat .NET:ssä: [Linkki](https://learn.microsoft.com/en-us/dotnet/api/system.midpointrounding?view=net-7.0)
- Keskusteluja pyöristämisestä StackOverflow'ssa: [Linkki](https://stackoverflow.com/questions/tagged/rounding+powershell)
