---
title:                "C: Kahden päivämäärän vertailu"
programming_language: "C"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Miksi

Vertaamalla kahta päivämäärää voidaan selvittää, kumpi niistä on aikaisempi tai myöhempi. Tämä voi olla tarpeellista esimerkiksi tarkistaessa, onko jokin tapahtuma jo ohitse vai onko se vasta tulossa.

## Miten

Vertailun tekemiseen päivämäärien välillä tarvitaan useita toimintoja ja muuttujia. Seuraavassa on esimerkiksi yksinkertainen koodilohko, joka vertaa kahta syötettyä päivämäärää ja tulostaa tiedon siitä, kumpi niistä on myöhempi:

```C
#include <stdio.h>
#include <stdlib.h>

int main() {
  // Alustetaan päivämäärien muuttujat
  int day1, month1, year1, day2, month2, year2;
  
  // Pyydetään käyttäjältä syötteitä
  printf("Syötä ensimmäinen päivämäärä muodossa päivä/kuukausi/vuosi: ");
  scanf("%d/%d/%d", &day1, &month1, &year1);
  
  printf("Syötä toinen päivämäärä muodossa päivä/kuukausi/vuosi: ");
  scanf("%d/%d/%d", &day2, &month2, &year2);
  
  // Suoritetaan vertailu ja tulostetaan tieto
  if (year1 > year2) {
    printf("Ensimmäinen päivämäärä on myöhempi.");
  } else if (year1 < year2) {
    printf("Toinen päivämäärä on myöhempi.");
  } else {
    // Vertaillaan tarvittaessa myös kuukausia ja päiviä
    if (month1 > month2) {
      printf("Ensimmäinen päivämäärä on myöhempi.");
    } else if (month1 < month2) {
      printf("Toinen päivämäärä on myöhempi.");
    } else {
      if (day1 > day2) {
        printf("Ensimmäinen päivämäärä on myöhempi.");
      } else if (day1 < day2) {
        printf("Toinen päivämäärä on myöhempi.");
      } else {
        printf("Molemmat päivämäärät ovat samat.");
      }
    }
  }
  return 0;
}
```

Esimerkki tulosteesta syötteillä "12/5/2020" ja "8/6/2020":

```
Syötä ensimmäinen päivämäärä muodossa päivä/kuukausi/vuosi: 12/5/2020
Syötä toinen päivämäärä muodossa päivä/kuukausi/vuosi: 8/6/2020
Ensimmäinen päivämäärä on myöhempi.
```

## Syvemmälle

Päivämäärien vertaaminen voi olla monimutkaista, erityisesti jos halutaan huomioida esimerkiksi karkausvuodet tai eri aikavyöhykkeiden väliset erot. On tärkeää muistaa, että pelkistetty esimerkkimme ei välttämättä toimi kaikissa tilanteissa ja ohjelman tulee olla tarpeeksi joustava selviytyäkseen erilaisista syötteistä.

## Katso myös

- [How to Compare Dates in C](https://www.programiz.com/c-programming/examples/difference-dates)
- [Date Arithmetic in C](https://www.programiz.com/c-programming/examples/date-arithmetic)