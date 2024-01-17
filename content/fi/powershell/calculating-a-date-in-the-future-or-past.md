---
title:                "Laskeminen tulevaisuuden tai menneen päivän määrittäminen tietokoneohjelmoinnissa"
html_title:           "PowerShell: Laskeminen tulevaisuuden tai menneen päivän määrittäminen tietokoneohjelmoinnissa"
simple_title:         "Laskeminen tulevaisuuden tai menneen päivän määrittäminen tietokoneohjelmoinnissa"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/powershell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

##Mitä & Miksi?
Kun ohjelmoijat puhuvat ajan laskemisesta tulevaisuudessa tai menneisyydessä, kyseessä on yhtä aikaa yksinkertainen ja tärkeä toiminto. Se mahdollistaa aikojen vertailun ja tarkkaan määritellyn aikahorisontin luomisen. Ohjelmoijat tekevät sitä monista syistä, kuten aikaleimojen luomiseen tapahtumille ja tiedon tallentamiseen tietokantoihin.

##Kuinka toimia:
```PowerShell
# Laske päivä 30 päivää eteenpäin nykyisestä päivästä
(Get-Date).AddDays(30)

# Palauta päivämäärän eri osat erillisinä muuttujina
$Date = Get-Date
$Day = $Date.Day
$Month = $Date.Month
$Year = $Date.Year
```

**Tuloste:**
```
29.03.2022 11:37:26
```
```
Day: 29
Month: 3
Year: 2021
```

##Syvällisemmin:
Yksi syy ajan laskemisen tärkeyteen on historiansa. Ennen kehittyneitä tietokoneita ihmisten täytyi laskea päivät ja ajat manuaalisesti. Ohjelmoijat ovat luoneet erityisiä algoritmeja, jotka mahdollistavat tarkkojen päivämäärien laskemisen tarkasti ja nopeasti. Lisäksi on olemassa muita tapoja laskea päiviä, kuten käyttämällä Excelin tai Pythonin kaltaisia työkaluja.

##Katso myös:
- [MDN Web Docs: Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [PowerShell Documentation: Date and Time](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.1)