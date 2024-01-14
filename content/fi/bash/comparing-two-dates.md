---
title:                "Bash: Kahden päivämäärän vertailu"
programming_language: "Bash"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Miksi vertailla kahta päivämäärää?

Päivämäärän vertaaminen on yleinen tehtävä Bash-ohjelmoinnissa. Oli kyseessä sitten tiedostoja tai tapahtumia sisältävä lokitiedosto, päivämääriä voidaan vertailla esimerkiksi löytääksemme viimeisimmän tai vanhimman päivämäärän. Se voi myös auttaa meitä seulomaan tietoa ja tekemään päätöksiä perustuen päivämäärään.

## Näin vertailet kahta päivämäärää Bash-ohjelmoinnissa

Käytämme ensin `date` -komennon `+%s` -parametria muuntaaksemme päivämäärät unix-aikaleimoiksi. Tämä helpottaa vertailua.

```Bash
date1=$(date -d "2020-01-01" +%s)
date2=$(date -d "2021-01-01" +%s)

if [ $date1 -gt $date2 ]; then
    echo "2020 was a leap year"
else
    echo "2021 will be a leap year"
fi
```

Tässä esimerkissä käytämme loogista vertailua `-gt`, joka tarkoittaa "greater than" (suurempi kuin). Voimme myös käyttää muita loogisia operaattoreita, kuten `-eq` (yhtä suuri kuin) tai `-lt` (pienempi kuin).

## Syvällisempää tietoa päivämäärien vertailusta

Unix-aikaleimoja käytetään yleisesti vertailussa, koska ne ovat helppoja käsitellä ja tarkkoja. Muutamia muita hyödyllisiä komennoja päivämäärien muuntamiseen unix-aikaleimoiksi ovat `date -j` ja `date -u`. Lisätietoa näistä saat `man date`-komentoa käyttämällä.

Unix-aikaleimojen lisäksi päivämäärien vertailuun voi käyttää myös `date -d` -komennon `-v` -parametriä. Tämä mahdollistaa tietyn ajan lisäämisen tai vähentämisen päivämäärästä.

## Katso myös

- [Bash -päivämäärä-lähteet (englanniksi)](https://www.tutorialspoint.com/unix_commands/date.htm)
- [Pythonin datetime-kirjaston käyttäminen päivämäärien käsittelyyn (englanniksi)](https://realpython.com/python-datetime/)
- [Unix-aikaleimojen selittäminen (englanniksi)](https://www.unixtimestamp.com/)