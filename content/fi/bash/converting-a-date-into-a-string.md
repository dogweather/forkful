---
title:                "Bash: Päivämäärän muuttaminen merkkijonoksi"
simple_title:         "Päivämäärän muuttaminen merkkijonoksi"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi muuntaa päivämäärä merkkijonoksi?

Joskus haluat ehkä näyttää päivämäärän merkkijonona eri syistä, kuten käyttäjän ystävällisemmäksi näyttämiselle tai tietojen tallentamiselle tietokantaan. Tässä blogikirjoituksessa kerromme, miten voit muuntaa päivämäärän Bash-ohjelmoinnissa merkkijonoksi.

## Ohjeet

Bash-ohjelmointikielessä on olemassa useita tapoja muuntaa päivämäärä merkkijonoksi, mutta tässä esittelemme yhden yksinkertaisen tavan käyttää komentoa `date` ja sen optiota `%F`, joka näyttää päivämäärän muodossa YYYY-MM-DD.

Ensimmäisenä luodaan muuttuja `today`, johon tallennetaan tämänhetkinen päivämäärä `date`-komennolla.

```Bash
today=$(date +%F)
```

Sitten tulostetaan muuttujan `today` arvo, joka on päivämäärä merkkijonona.

```Bash
echo $today
```

Tulostettu merkkijono näyttää esimerkiksi tältä: `2021-08-19`.

## Syvällisempi sukellus

`date`-komento tarjoaa monia muita optioita päivämäärän muotoiluun, kuten `%d` päivän numerona ja `%b` kuukauden lyhyenä nimenä. Voit myös muuttaa päivämäärän paikalliseen aikavyöhykkeeseen lisäämällä optio `%Z`.

Voit lukea lisätietoja `date`-komennon optioista Bashin manuaalioppaasta komennolla `man date` tai Bashin kehittäjäsivuilta.

## Katso myös

- [Bashin manuaaliopas päivämäärän merkkaamiseen] (https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html#Shell-Parameters) 
- [Bashin kehittäjän sivusto date-komennosta] (https://www.gnu.org/software/bash/manual/html_node/Bash-Variables.html#Bash-Variables)