---
title:                "Fish Shell: Ajan laskeminen tulevaisuudessa tai menneisyydessä"
simple_title:         "Ajan laskeminen tulevaisuudessa tai menneisyydessä"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi

Erityisesti sovelluskehittäjien ja tietotekniikan harrastajien on usein tarpeen laskea tietty päivämäärä tulevaisuudessa tai menneisyydessä. Tämä voi johtua esimerkiksi muistutusten asettamisesta tai aikaleimojen tallentamisesta tiedostoihin. Joten, miksi ei tekisi sitä helposti Fish Shell -ohjelmoinnin avulla? Lue eteenpäin löytääksesi kuinka.

## Kuinka

Fish Shell tarjoaa monia hyödyllisiä toimintoja päivämäärän laskemiseen tulevaisuudessa tai menneisyydessä. Yksi tapa tehdä tämä on käyttämällä `date` -komennon `--date` -asetusta yhdessä halutun päivän tai ajan määrittämisen kanssa. Esimerkiksi, jos haluamme laskea päivän päivämäärästä kolme päivää eteenpäin, voimme käyttää seuraavaa komentoa:

```Fish Shell
date --date="3 days" "+%d/%m/%Y"
```

Mikä tuottaa seuraavan tulosteen:

```
26/09/2021
```

Vastaavasti voimme laskea päivämäärän kolme päivää taaksepäin käyttämällä `--date` -asetusta negatiivisen numeron kanssa:

```Fish Shell
date --date="-3 days" "+%d/%m/%Y"
```

Joka tuottaa tuloksen:

```
20/09/2021
```

## Syventyminen

Fish Shellin `date` -komennolla on monia muita käyttökelpoisia vaihtoehtoja päivämäärän määrittämiseen ja muotoiluun. Esimerkiksi voit käyttää `--date` -asetusta lisäämällä tai vähentämällä päiviä, viikkoja, kuukausia tai vuosia tai jopa määrittämällä tietyn päivämäärän, kuten `25/12/2021`. Voit myös muuttaa tulosteen muotoa lisäämällä `%` -merkin jälkeen halutun muotoilumerkin, kuten `%A` päivän nimen näyttämiseksi. Lisätietoja vaihtoehdoista löytyy `date` -komennon manuaalisivulta.

## Katso myös

Lisätietoja Fish Shellin `date` -komennosta ja sen vaihtoehdoista löytyy seuraavista linkeistä:

- Manuaalisivu: `man date` (englanniksi)
- Fish Shellin dokumentaatio: https://fishshell.com/docs/current/cmds/date.html (englanniksi)
- Linux-käyttäjille: https://www.howtogeek.com/412383/how-to-use-linuxs-datum-date-command/ (englanniksi)
- Mac-käyttäjille: https://www.howtogeek.com/281459/how-to-use-the-date-command-on-linux/ (englanniksi)