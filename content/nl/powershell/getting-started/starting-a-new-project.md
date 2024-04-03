---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:28.085806-07:00
description: "Hoe: PowerShell maakt het starten van een nieuw project eenvoudig. Je\
  \ wilt misschien een map voor je project maken en een git-repository instellen.\
  \ Hier\u2026"
lastmod: '2024-03-13T22:44:51.029847-06:00'
model: gpt-4-0125-preview
summary: PowerShell maakt het starten van een nieuw project eenvoudig.
title: Een nieuw project starten
weight: 1
---

## Hoe:
PowerShell maakt het starten van een nieuw project eenvoudig. Je wilt misschien een map voor je project maken en een git-repository instellen. Hier is hoe:

```PowerShell
# Maak een nieuwe map voor je project
New-Item -Path 'C:\MijnProjecten\NieuweCooleApp' -ItemType Directory

# Navigeer naar je nieuwe map
Set-Location -Path 'C:\MijnProjecten\NieuweCooleApp'

# Initialiseer een nieuwe git-repository als je versiebeheer gebruikt
git init
```

Voorbeeldoutput:
```
    Directory: C:\MijnProjecten

Modus                 LaatsteSchrijfTijd    Lengte Naam
----                 -------------         ------ ----
d-----          1/1/2023   12:00 AM                NieuweCooleApp
Initialized empty Git repository in C:/MijnProjecten/NieuweCooleApp/.git/
```

## Dieper Duiken
PowerShell is sinds de introductie in 2006 de go-to scripttaal geweest voor Windows-automatisering. Een nieuw project met PowerShell creëren gaat niet alleen over het maken van mappen; het is een ritueel voor het vaststellen van projectomvangen, het definiëren van scripts, of het voorbereiden van geautomatiseerde taken.

Hoewel PowerShell een favoriet is in de Windows-wereld, vertrouwen Unix-achtige gebruikers vaak op 'bash' of 'zsh' voor vergelijkbare taken. Toch, met de komst van PowerShell Core, heeft PowerShell zich in de ring van multiplatform gestapt, waardoor scripten en automatisering over platforms mogelijk is.

Diepgeworteld in het ontwerp van PowerShell is de objectgeoriënteerde aard, waarbij cmdlets (uitgesproken als command-lets) objecten uitvoeren. Cmdlets zoals `New-Item` creëren niet alleen bestanden of mappen; ze bouwen objecten die je scripts kunnen aanspreken. Een nieuwe projectopzet kan inclusief zijn voor het vaststellen van een mappenstructuur, het creëren van een README, het instellen van een .gitignore-bestand, of zelfs het templaten van initiële codebestanden.

Het implementeren van een projectopzet routine in PowerShell kan tal van cmdlets benutten, van bestandsmanipulatie (`New-Item`) tot omgevingsconfiguratie (`Set-Location`). Deze combineren met de scriptmogelijkheden van PowerShell kan krachtige opzet-scripts creëren die dienen als projectstarters, waardoor je projectstructuren met minimale moeite worden uitgestempeld.

## Zie Ook
- [PowerShell Scripting](https://docs.microsoft.com/en-us/powershell/scripting/overview)
- [Pro Git Boek](https://git-scm.com/book/en/v2)
- [GitHub's Hallo Wereld](https://guides.github.com/activities/hello-world/)
- [PowerShell Core op GitHub](https://github.com/PowerShell/PowerShell)
