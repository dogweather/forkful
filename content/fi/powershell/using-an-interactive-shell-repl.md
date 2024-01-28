---
title:                "Interaktiivisen komentotulkin (REPL) käyttö"
date:                  2024-01-26T04:16:36.469903-07:00
model:                 gpt-4-0125-preview
simple_title:         "Interaktiivisen komentotulkin (REPL) käyttö"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Interaktiivinen kuori eli Read-Eval-Print Loop (REPL) mahdollistaa PowerShell-komentojen kirjoittamisen ja välittömän palautteen saamisen. Ohjelmoijat käyttävät sitä koodinpätkien nopeaan testaamiseen, vianetsintään tai uusien komentojen oppimiseen kirjoittamatta täydellistä skriptiä.

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
