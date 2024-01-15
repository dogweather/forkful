---
title:                "Kahden päivämäärän vertailu"
html_title:           "Arduino: Kahden päivämäärän vertailu"
simple_title:         "Kahden päivämäärän vertailu"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Miksi?

On monia käytännön tilanteita, joissa on tarpeen vertailla kahta eri päivämäärää. Yleisimmin tämä tehdään esimerkiksi ohjelmoidessa automatisoituja järjestelmiä, jotka käyttävät päivämääriä tärkeiden tapahtumien ja muiden tietojen vertailuun.

## Miten?

Vertaillessa kahta päivämäärää, on tärkeää muistaa, että päivämäärät tallennetaan Arduinoon joko kokonaislukuina tai merkkijonoina. Jos päivämäärät ovat merkkijonoja, ne täytyy ensin muuntaa kokonaisluvuiksi ennen vertailua. Esimerkiksi:

```Arduino
// Tallennetaan päivämäärät merkkijonoina
String date1 = "2021-05-01";
String date2 = "2021-05-10";

// Muunnetaan merkkijonot kokonaisluvuiksi
int date1_int = atoi(date1.c_str());
int date2_int = atoi(date2.c_str());

// Vertaillaan päivämääriä
if (date1_int < date2_int) {
  Serial.println("Ensimmäinen päivämäärä on aiempi!");
} else if (date1_int > date2_int) {
  Serial.println("Toinen päivämäärä on aiempi!");
} else {
  Serial.println("Päivämäärät ovat samat!");
}
```

**Tulostus:**
```
Ensimmäinen päivämäärä on aiempi!
```

Kokonaislukumuodossa olevia päivämääriä voidaan vertailla suoraan käyttämällä matemaattisia operaattoreita, kuten esimerkiksi:

```Arduino
// Tallennetaan päivämäärät kokonaislukuina
int date1 = 20210501;
int date2 = 20210510;

// Vertaillaan päivämääriä
if (date1 < date2) {
  Serial.println("Ensimmäinen päivämäärä on aiempi!");
} else if (date1 > date2) {
  Serial.println("Toinen päivämäärä on aiempi!");
} else {
  Serial.println("Päivämäärät ovat samat!");
}
```

**Tulostus:**
```
Ensimmäinen päivämäärä on aiempi!
```

## Syvemmälle

Päivämäärävertailuun liittyy muutamia tärkeitä seikkoja, joita kannattaa pitää mielessä. Ensinnäkin, päivämäärät tallennetaan tavallisesti päiväysjärjestelmään, eli allekirjoitusnumero muuttuu päivämäärän kasvaessa. Tästä syystä esimerkiksi vuosi 2021 tarkoittaa kokonaislukua 2021 ja maaliskuun kolmattakymmenettä ensimmäistä päivää kokonaislukua 20210330. Tämä tulee ottaa huomioon päivämäärien vertailussa.

Toiseksi, jos päivämäärät ovat tallennettuina merkkijonoina, tulee niiden olla samassa muodossa vertailua varten. Esimerkiksi kirjoitettaessa päivämääriä järjestelmään, jossa päivä ja kuukausi ovat eroteltu välilyönnillä, tulee muuttaa myös vertailtavat päivämäärät samanlaiseen muotoon.

## Katso myös

- [Arduino Reference - atoi()](https://www.arduino.cc/reference/en/language/functions/conversion/atoi/)
- [C++ Reference - c_str()](https://www.cplusplus.com/reference/string/string/c_str/)