---
date: 2024-01-20 17:39:23.851647-07:00
description: "How to: | Kuinka: PowerShellissa merkkijonon muuttaminen pieniksi kirjaimiksi\
  \ on yksinkertaista. K\xE4yt\xE4 `.ToLower()` -metodia tai `ToLowerInvariant()`\u2026"
lastmod: '2024-04-05T22:38:57.379330-06:00'
model: gpt-4-1106-preview
summary: "| Kuinka: PowerShellissa merkkijonon muuttaminen pieniksi kirjaimiksi on\
  \ yksinkertaista. K\xE4yt\xE4 `.ToLower()` -metodia tai `ToLowerInvariant()` -metodia,\
  \ jos haluat varmistaa kielen alueellisten asetusten huomiotta j\xE4tt\xE4misen."
title: Merkkijonon muuntaminen pieniksi kirjaimiksi
weight: 4
---

## How to: | Kuinka:
PowerShellissa merkkijonon muuttaminen pieniksi kirjaimiksi on yksinkertaista. Käytä `.ToLower()` -metodia tai `ToLowerInvariant()` -metodia, jos haluat varmistaa kielen alueellisten asetusten huomiotta jättämisen. 

```PowerShell
# Käytetään .ToLower() -metodia
$exampleString = "Hello, World!"
$lowerCaseString = $exampleString.ToLower()
$lowerCaseString
```

Sample output:
```
hello, world!
```

```PowerShell
# Käytetään ToLowerInvariant() -metodia
$exampleString = "HELLO, WORLD!"
$lowerCaseString = $exampleString.ToLowerInvariant()
$lowerCaseString
```

Sample output:
```
hello, world!
```

## Deep Dive | Syväsukellus:
Ennen PowerShellin aikaa käyttäjät turvautuivat alhaisemman tason kielissä, kuten C:ssä, käsin kirjoitetut koodit merkkijonon muuttamiseen pieniksi kirjaimiksi. `.ToLower()` ja `.ToLowerInvariant()` metodit ovat olleet osa .NET-kirjastoa alusta asti, ja ne helpottavat työtä antamalla valmiin toiminnallisuuden.

Vaihtoehtoisesti, voit käyttää `tr` -komentoa Unix-pohjaisissa järjestelmissä tai koetella tekemään muunnoksen käyttäen ASCII-arvoja, mutta nämä menetelmät ovat mutkikkaampia ja alttiimpia virheille.

`.ToLower()` käyttää kulttuurispecifistä pienten kirjainten mappia, mikä voi johtaa eroihin riippuen kulttuuriasetuksista. `.ToLowerInvariant()` puolestaan käyttää invarianttia kulttuuria, mikä tuottaa yhdenmukaisia tuloksia riippumatta alueellisista asetuksista.

## See Also | Katso Myös:
- Microsoft's official documentation on ToLower() and ToLowerInvariant(): [Linkki](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower)
- Stack Overflow discussions on string manipulation in PowerShell: [Linkki](https://stackoverflow.com/questions/tagged/powershell+string+tolower)
