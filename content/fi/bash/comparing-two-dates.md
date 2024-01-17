---
title:                "Kahden päivämäärän vertailu"
html_title:           "Bash: Kahden päivämäärän vertailu"
simple_title:         "Kahden päivämäärän vertailu"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Mitä Ja Miksi? 
Vertaillessaan kahta päivämäärää, ohjelmoijat voivat selvittää aikajärjestystä tai laskea kuinka monta päivää on kulunut kahden tapahtuman välillä. Tätä taitoa käytetään usein aikaleimojen tarkistamisessa tai aikaperusteisten laskelmien suorittamisessa.

## Kuinka: 
Koodiesimerkit ja tulosteet alla olevilla ```Bash``` koodilohkoilla.

### Esimerkki 1:
```Bash 
päivä1 = date -d "11/18/2021" +%s 
päivä2 = date -d "11/15/2021" +%s 
eros = $(($(($päivä1 - $päivä2)) / 86400))
echo "Päiviä päivämäärän 11/15/2021 ja 11/18/2021 välillä on $eros päivänä."
```
Tuloste:
```
Päiviä päivämäärän 11/15/2021 ja 11/18/2021 välillä on 3 päivää.
```

### Esimerkki 2:
```Bash
päivä1 = date -d "now" +%s 
päivä2 = date -d "12/25/2021" +%s 
jos [ $((päivä1 <päivä2)) -eq 1 ] 
sitten echo "Jouluaattoon on $((($(($päivä2 - $päivä1))) / 86400)) päivää."
```
Tulote: 
```
Jouluaattoon on 39 päivää.
```

## Syväsukellus:
Vertaamalla kahta päivämäärää voimme selvittää mitkä päivämäärät ovat aiemmin ja mitkä myöhemmin. Tämä on erityisen hyödyllistä päivämäärän aikaleimojen tarkistamisessa, kuten tapahtumien järjestystä tai keston laskemista. Bashissa vertaamme päivämääriä UNIX-aikaleimoiksi, jotka edustavat sekunteja, jotka ovat kuluneet 1. tammikuuta 1970 klo 00.00 UTC:sta tähän päivään asti. Toinen tapa vertailla päivämääriä on käyttää dateutil-moduulia Pythonissa.

## Katso myös:
- "How to Compare Dates in Bash" (https://www.baeldung.com/linux/bash-compare-dates)
- "The dateutil Module in Python" (https://dateutil.readthedocs.io/en/stable/)
- "Understanding UNIX Time" (https://www.epochconverter.com/)