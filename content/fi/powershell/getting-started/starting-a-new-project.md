---
date: 2024-01-20 18:04:37.576833-07:00
description: "How to: - Kuinka tehd\xE4\xE4n: Powershellill\xE4 uuden projektin pystytys\
  \ alkaa usein hakemiston luomisella ja sinne siirtymisell\xE4. Esimerkiksi - alla\
  \ koodi uuden\u2026"
lastmod: '2024-04-05T22:38:57.394963-06:00'
model: gpt-4-1106-preview
summary: "- Kuinka tehd\xE4\xE4n: Powershellill\xE4 uuden projektin pystytys alkaa\
  \ usein hakemiston luomisella ja sinne siirtymisell\xE4. Esimerkiksi - alla koodi\
  \ uuden hakemiston tekemiseen ja siell\xE4 projektin aloitukseen."
title: Uuden projektin aloittaminen
weight: 1
---

## How to: - Kuinka tehdään:
Powershellillä uuden projektin pystytys alkaa usein hakemiston luomisella ja sinne siirtymisellä. Esimerkiksi - alla koodi uuden hakemiston tekemiseen ja siellä projektin aloitukseen:

```PowerShell
New-Item -Path 'C:\Projects\MyNewProject' -ItemType Directory
Set-Location -Path 'C:\Projects\MyNewProject'
# Initiate a new Git repository, if you use version control
git init
# Create a new README.md file to describe your project
New-Item -Name 'README.md' -ItemType File
```

Tässä simppeli kaiku siitä, mitä juuri tehtiin:

```
Directory: C:\Projects\MyNewProject

Mode                LastWriteTime         Length Name
----                -------------         ------ ----
d-----         1/1/2023   12:00 AM                .git
-a----         1/1/2023   12:00 AM              0 README.md
```

## Deep Dive - Syväsukellus:
Projektin aloittamisessa on historiaa. Ennen vanhaan kaikki tehtiin käsin, folderista kooditiedostoon. Nyt meillä on kehitysympäristöjä, frameworkkeja ja paketinhallinnan työkaluja, esimerkkeinä Visual Studio ja NuGet PowerShellissa. Versiohallinta on käytännössä pakollinen ja suosituin työkalu on Git.

Kun aloitat projektin, PowerShell-scripteilläkin voi olla paketinhallinnan tarpeita. PowerShellGet-moduuli on siihen tarkoitukseen, ja `New-ModuleManifest` luodaan moduuleille.

Uusien projektien rakenne ja tarvittavat tiedostot vaihtelevat tyypin ja käyttötarkoituksen mukaan. Esimerkiksi, moduuli-projekteilla on omat standardinsa verrattuna skripti-kokoelmiin.

## See Also - Katso Myös:
- [PowerShellGet](https://docs.microsoft.com/en-us/powershell/module/powershellget/?view=powershell-7.1)
- [Käyttöohjeet versiohallintaan Gitin avulla](https://git-scm.com/book/fi/v2)
- [Microsoftin PowerShell-dokumentaatio](https://docs.microsoft.com/fi-fi/powershell/)
- [Visual Studio Code - erinomainen editori PowerShellille](https://code.visualstudio.com/docs/languages/powershell)
