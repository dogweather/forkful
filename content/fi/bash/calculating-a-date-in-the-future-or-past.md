---
title:                "Tulevaisuuden tai menneen päivämäärän laskeminen"
html_title:           "Bash: Tulevaisuuden tai menneen päivämäärän laskeminen"
simple_title:         "Tulevaisuuden tai menneen päivämäärän laskeminen"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi

Bash-ohjelmoinnilla voi laskea päivämäärän tulevaisuudessa tai menneisyydessä. Tämä voi olla hyödyllistä esimerkiksi silloin, kun halutaan automatisoida ajastetun tehtävän suorittaminen tietyllä päivämäärällä.

## Kuinka tehdä

Bashissa päivämäärälaskentaan voidaan käyttää `date`-komennon `-d`-parametria. Esimerkiksi tulevaisuuden päivämäärän laskemiseen voidaan käyttää seuraavaa koodia:

```Bash
# Tallentaa tulevan päivämäärän muuttujaan "paivays"
paivays=$(date -d "+10 days" +%m/%d/%Y) 
echo $paivays
# Tulostaa esimerkkilähtöpäivämäärän lisättynä 10 päivää
# Output: 04/28/2021
```

Menneisyyden päivämäärän laskeminen toimii samalla periaatteella, mutta käytetään negatiivista lukua päivien määrässä:

```Bash
# Tallentaa menneen päivämäärän muuttujaan "paivays"
paivays=$(date -d "-5 days" +%m/%d/%Y) 
echo $paivays
# Tulostaa esimerkkilähtöpäivämäärän vähennettynä 5 päivää
# Output: 04/13/2021
```

## Syvemmälle

Bashin `date`-komennolla voidaan tehdä monimutkaisempia päivämäärälaskelmia, kuten lisätä tai vähentää vuosia, kuukausia tai viikkoja. Lisätietoa ja käyttöesimerkkejä `date`-komennosta löytyy [Bashin dokumentaatiosta](https://www.gnu.org/software/bash/manual/html_node/Bash-Variables.html#Bash-Variables) sekä [Linuxin `date`-komennon manuaalisivulta](https://linux.die.net/man/1/date).

## Katso myös

- [Bashin dokumentaatio](https://www.gnu.org/software/bash/manual/)
- [Linuxin `date`-komennon manuaalisivu](https://linux.die.net/man/1/date)