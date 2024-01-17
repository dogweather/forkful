---
title:                "Päivämäärän muuntaminen merkkijonoksi"
html_title:           "Fish Shell: Päivämäärän muuntaminen merkkijonoksi"
simple_title:         "Päivämäärän muuntaminen merkkijonoksi"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?

Päivämäärä-muuttujan muuttaminen merkkijonoksi on yleinen ohjelmoinnin tehtävä. Tällä tavalla ohjelmoijat voivat esittää päivämääriä selkeämmässä ja helpommin ymmärrettävässä muodossa.

## Kuinka tehdä:

Fish Shell -ympäristössä päivämäärän muuttaminen merkkijonoksi on helppoa. Tässä on muutama esimerkki koodista ja näiden esimerkkien tulostuksista.

```Fish Shell
date +%d-%m-%Y
02-06-2020
```

```Fish Shell
date "+Today is %A, %B %d, %Y"
Today is Tuesday, June 02, 2020
```

```Fish Shell
date "+%A, %B %d, %Y"
Tuesday, June 02, 2020
```

```Fish Shell
date -j -f "%Y%m%d%H%M%S" "20200602211111" "+%B %d, %Y"
June 02, 2020
```

## Syvemmälle pinnalle:

Päivämäärän muuttaminen merkkijonoksi on ollut tärkeä osa ohjelmointia jo pitkään. Monissa muissa ohjelmointikielissä on omat erilaiset funktiot ja komentorivin apuohjelmat joilla tämä voidaan toteuttaa. Fish Shell tarjoaa kuitenkin yksinkertaisen ja kätevän tavan käsitellä päivämääriä. 

Jos et käytä Fish Shelliä, voit käyttää esimerkiksi Pythonin tai JavaScriptin date-funktiota päivämäärän muuttamiseen merkkijonoksi.

Päivämäärän muuttaminen merkkijonoksi Fish Shellissä tapahtuu käyttämällä date-komentoa yhdessä erilaisten muotoilu- ja aikamuotojen kanssa.

## Katso myös:

- [Fish Shell: date manuaalisivu](https://fishshell.com/docs/current/cmds/date.html)
- [Fish Shell: almanakka-sivu](https://fishshell.com/docs/current/index.html#module-almanakka)