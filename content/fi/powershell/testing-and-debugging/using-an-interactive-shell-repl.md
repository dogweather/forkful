---
date: 2024-01-26 04:16:36.469903-07:00
description: "Interaktiivinen kuori eli Read-Eval-Print Loop (REPL) mahdollistaa PowerShell-komentojen\
  \ kirjoittamisen ja v\xE4litt\xF6m\xE4n palautteen saamisen. Ohjelmoijat\u2026"
lastmod: '2024-03-13T22:44:56.783885-06:00'
model: gpt-4-0125-preview
summary: "Interaktiivinen kuori eli Read-Eval-Print Loop (REPL) mahdollistaa PowerShell-komentojen\
  \ kirjoittamisen ja v\xE4litt\xF6m\xE4n palautteen saamisen."
title: "Interaktiivisen komentotulkin (REPL) k\xE4ytt\xF6"
weight: 34
---

## Kuinka:
Käynnistä PowerShell ja olet REPL:ssä. Kokeile `Get-Date` Cmdlet:

```PowerShell
PS > Get-Date
```

Näet senhetkisen päivämäärän ja kellonajan tulosteen:

```PowerShell
Keskiviikko, maaliskuu 31, 2023 12:34:56 IP
```

Ketjuta komentoja. Lajitellaan prosessit muistinkäytön mukaan:

```PowerShell
PS > Get-Process | Sort-Object WS -Descending | Select-Object -First 5
```

Tämä tulostaa viisi prosessia työjoukon koon (muistinkäyttö) mukaan.

## Syväsukellus
PowerShellin REPL juontaa juurensa Unix-kuoreen ja muihin dynaamisen kielen kuoriin, kuten Pythonin kuoriin. Se on yksittäisen käyttäjän vuorovaikutteinen komentojen suoritusympäristö. Toisin kuin koottavassa kielessä, jossa kirjoitat kokonaisia sovelluksia ja sitten kokoat ne, REPL-ympäristössä voit kirjoittaa ja suorittaa koodia yksi rivi kerrallaan. PowerShell tukee myös skriptien suorittamista suurempia tehtäviä varten.

Vaihtoehtoja Windowsille ovat muun muassa komentorivi tai muiden kielikohtaisten REPL:ien käyttö, kuten IPython. Unix/Linux-maailmassa kuoret, kuten bash tai zsh, palvelevat samankaltaista toimintoa.

PowerShellin toteutus käyttää isäntäsovellusta kuoren suorittamiseen. Vaikka Windowsissa yleisin on PowerShell.exe, myös muut, kuten Integrated Scripting Environment (ISE) tai Visual Studio Coden integroitu terminaali, voivat toimia isäntänä.

## Katso myös
- [Tietoa PowerShellistä](https://docs.microsoft.com/en-us/powershell/scripting/overview)
- [StackOverflow: PowerShell](https://stackoverflow.com/questions/tagged/powershell)
