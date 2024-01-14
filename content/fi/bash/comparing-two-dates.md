---
title:    "Bash: Kahden päivämäärän vertailu"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Miksi vertailla kahta päivämäärää
Päivämäärien vertailu on tärkeä osa monia ohjelmia ja skriptejä. Se voi auttaa näyttämään, onko jokin päivämäärä tulevaisuudessa tai menneisyydessä ja auttaa tekemään päätöksiä sen perusteella. Lisäksi päivämäärien vertailu mahdollistaa tiettyjen päivämäärien tai ajanjaksojen tarkastelun ja analysoinnin.

# Miten vertailla kahta päivämäärää
Vertaillessasi kahta päivämäärää Bash-ohjelmointiympäristössä käytät komentoa `date` ja `if`-lauseketta. Tässä on esimerkki:
```Bash
date1="2021-01-01"
date2="2021-02-01"
if [ "$date1" \< "$date2" ]; then
    echo "Ensimmäinen päivämäärä on ennen toista."
else
    echo "Ensimmäinen päivämäärä on jälkeen toista."
fi
```
Tässä esimerkissä olemme asettaneet kaksi päivämäärää muuttujiin `date1` ja `date2`ja vertailleet niitä `if`-lausekkeen avulla. Kun suoritat tämän koodin, tulostetaan vastaava lause päivämäärien järjestyksen mukaan.

## Syväsukellus
Päivämäärien vertaileminen Bash-skriptissä voi olla hieman monimutkaisempaa, jos haluat ottaa huomioon esimerkiksi vuodet tai kuukaudet. Tällöin voit käyttää `date`-komentoon vaihtoehtoisia vaihtoehtoja, kuten `%Y` vuosien vertailuun ja `%m` kuukausien vertailuun. Tässä on esimerkki:
```Bash
date1="2021-01-01"
date2="2020-12-01"
if [ $(date -d "$date1" +%Y%m) -gt $(date -d "$date2" +%Y%m) ]; then
    echo "Ensimmäinen päivämäärä on jälkeen toista."
else
    echo "Ensimmäinen päivämäärä on ennen toista."
fi
```
Tässä olemme käyttäneet `date`-komenton `-d`-vaihtoehtoa antamaan päivämäärät halutussa muodossa ja `%Y%m`-vaihtoehtoa muuntamaan päivämäärien vertailukelpoiseen muotoon. Tämä esimerkki tulostaa vastaavan lausekkeen päivämäärien järjestyksen mukaisesti.

## Katso myös
- [Bashin `date`-komenton dokumentaatio](hhttps://www.gnu.org/software/coreutils/manual/html_node/birth_002dday-invocation.html#birth_002dday-invocation)
- [Muita Bashin päivämäärien vertailuesimerkkejä](https://stackoverflow.com/questions/3654/how-to-compare-two-dates-in-a-shell)