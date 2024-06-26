---
date: 2024-01-20 17:58:21.589017-07:00
description: "How to - N\xE4in se tehd\xE4\xE4n Voit etsi\xE4 ja korvata teksti\xE4\
  \ PowerShellissa k\xE4ytt\xE4m\xE4ll\xE4 `-replace` operaattoria."
lastmod: '2024-04-05T22:38:57.377679-06:00'
model: gpt-4-1106-preview
summary: "How to - N\xE4in se tehd\xE4\xE4n Voit etsi\xE4 ja korvata teksti\xE4 PowerShellissa\
  \ k\xE4ytt\xE4m\xE4ll\xE4 `-replace` operaattoria."
title: Tekstin etsiminen ja korvaaminen
weight: 10
---

## How to - Näin se tehdään
Voit etsiä ja korvata tekstiä PowerShellissa käyttämällä `-replace` operaattoria.

```PowerShell
# Etsi ja korvaa yksinkertainen esimerkki
$teksti = 'Hello Helsinki!'
$uusiTeksti = $teksti -replace 'Helsinki', 'Espoo'
$uusiTeksti
```

Output:
```
Hello Espoo!
```

Voit myös käyttää regex (säännöllisiä lausekkeita) monimutkaisempiin etsintöihin.

```PowerShell
# Regex esimerkki
$teksti = 'Tervetuloa vuoteen 2023!'
$korjattuTeksti = $teksti -replace '\d{4}', '2024'
$korjattuTeksti
```

Output:
```
Tervetuloa vuoteen 2024!
```

## Deep Dive - Syväsukellus
PowerShell on rakennettu .NET:in päälle, mikä mahdollistaa sen, että string korvaukset toimivat tehokkaasti. `-replace` operaattori käyttää .NET:n `Regex.Replace` metodia oletuksena.

Historiallista kontekstia kaipaaville: tehtävä on vanha kuin mikroaaltouuni. Unix-like järjestelmissä `sed` on klassinen komentorivityökalu tekstikorvausten tekemiseen, ja PowerShellin tekstin korvaamisen ominaisuudet ovat tavallaan sen henkiset perilliset.

Vaihtoehtoja? Voit tehdä samaa .NET:n `String.Replace` metodilla, mutta se ei tuki regexiä, joka on tarpeen monimutkaisemmissa tapauksissa.

## See Also - Katso myös
- Microsoftin virallinen dokumentaatio regexistä PowerShellissa: [about_Regular_Expressions](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_regular_expressions?view=powershell-7.3)
- `Get-Help about_Comparison_Operators` PowerShellissa lisätietoja `-replace` operaattorista
- Korvaamiseen liittyvät StackOverflow-keskustelut ja esimerkit: [StackOverflow: PowerShell Replace](https://stackoverflow.com/questions/tagged/powershell+replace)
