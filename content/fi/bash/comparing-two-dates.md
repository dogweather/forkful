---
title:                "Kahden päivämäärän vertaaminen"
html_title:           "Bash: Kahden päivämäärän vertaaminen"
simple_title:         "Kahden päivämäärän vertaaminen"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Vertaaminen kahden päivämäärän kesken on prosessi, jossa tutkitaan kahden eri ajanhetken suhteellista sijoittumista aikajanalla (onko toinen aikaisempi vai myöhäisempi kuin toinen). Ohjelmoijat tekevät tämän yleisesti lokaamaan tapahtumia aikajanalla, aikarajoitusten hallintaan tai tehdäkseen aikaleimojen välisiä matemaattisia laskelmia.

## Kuinka tehdä:

Voit verrata kahta päivämäärää käyttäen Bash-skriptauskieltä tässä esimerkissä:

```Bash
# Päivämäärien luominen
PVM1=$(date -d"2021-12-31" +%s)
PVM2=$(date -d"2022-01-01" +%s)

# Vertailu
if [ "${PVM1}" -gt "${PVM2}" ]; then
    echo "Päivämäärä 1 on suurempi (myöhempi) kuin päivämäärä 2."
elif [ "${PVM1}" -eq "${PVM2}" ]; then
    echo "Päivämäärät ovat samat."
else
    echo "Päivämäärä 2 on suurempi (myöhempi) kuin päivämäärä 1."
fi
```

## Syvällinen tarkastelu:

Historiallisesti päivämäärien vertailu ei ole aina ollut yksinkertainen prosessi johtuen erilaisista päivämäärämuodoista ja tietokonejärjestelmien erilaisesta käsittelystä. Bash on ollut yksi työkalu, joka on yksinkertaistanut päivämäärien käsittelyn Unix-pohjaisissa järjestelmissä.

Vaihtoehtoja Bashille päivämäärien vertailussa ovat mm. Python, Java, php, jne. Näissä voidaan käyttää eri päivämääräluokkia päivämäärävertailuun, joissa saatavilla on myös monimutkaisempia toimintoja.

Vertailu Bashissa tapahtuu muuntamalla päivämäärät Unix-aikaleimoiksi (sekuntien määrä 1.1.1970 klo 00.00.00 jälkeen) ja vertaamalla sitten näitä aikaleimoja.

## Katso myös:

- Bash-ohjekirja: http://www.gnu.org/software/bash/manual/bash.html
- Päivämääränkäsittelyn historiasta: https://en.wikipedia.org/wiki/System_time
- Alternatives - päivämäärän käsittely Pythonilla: https://docs.python.org/3/library/datetime.html