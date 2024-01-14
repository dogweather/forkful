---
title:                "Arduino: Kahden päivämäärän vertailu"
programming_language: "Arduino"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Miksi vertailla päivämääriä?
Usein Arduino-ohjelmoinnissa on tarpeen verrata kahta eri päivämäärää, esimerkiksi tarkistamaan onko nykyinen päivä ennen tai jälkeen tiettyä päivämäärää. Tämä voi olla hyödyllistä esimerkiksi erilaisten ajastettujen tapahtumien hallinnassa tai päivämäärän perusteella tapahtuvassa päätöksenteossa.

## Kuinka tehdä
Vertaillessa päivämääriä Arduino-ohjelmassa tulee ottaa huomioon päivämäärän esitystapa ja sen vertailun logiikka. Esimerkiksi jos päivämäärät on tallennettu muuttujiin, voidaan niitä vertailla seuraavalla tavalla:

```Arduino
// Tallennetaan päivämäärät muuttujiin
int date1 = 20210920;
int date2 = 20210810;

// Vertaillaan päivämääriä ja tulostetaan tulos sarjaporttiin
if(date1 < date2){
    Serial.println("Date1 on aiempi kuin Date2");
} else if(date1 > date2) {
    Serial.println("Date1 on myöhempi kuin Date2");
} else {
    Serial.println("Päivämäärät ovat samat");
}
```

Yllä olevassa koodissa ensin tallennetaan kaksi päivämäärää muuttujiin ja sitten verrataan niitä if-else rakenteessa. Arduino käyttää päivämäärän esittämiseen yleensä numeromuotoa YYYYMMDD, jossa vuosi on neljällä numerolla, kuukausi kahdella ja päivä kahdella numerolla.

## Syväsukellus
Arduino tarjoaa myös valmiita funktioita päivämäärän esittämiseen ja vertailuun. Esimerkiksi funktio `day()` palauttaa nykyisen päivän, `month()` nykyisen kuukauden ja `year()` nykyisen vuoden. Näitä funktioita voidaan käyttää hyödyksi päivämäärien vertailussa.

Esimerkiksi jos haluamme tulostaa nykyisen päivämäärän ja verrata sitä tiettyyn tulevaan päivämäärään, voimme käyttää seuraavaa koodia:

```Arduino
// Tallennetaan nykyinen päivämäärä muuttujiin
int currentDay = day();
int currentMonth = month();
int currentYear = year();

// Tulostetaan nykyinen päivämäärä sarjaporttiin
Serial.print("Nykyinen päivämäärä: ");
Serial.print(currentDay);
Serial.print("/");
Serial.print(currentMonth);
Serial.print("/");
Serial.println(currentYear);

// Vertaillaan nykyistä päivämäärää ja tulevaa päivämäärää
int futureDay = 20;
int futureMonth = 12;
int futureYear = 2021;

if(currentYear < futureYear || (currentYear == futureYear && currentMonth < futureMonth) || (currentYear == futureYear && currentMonth == futureMonth && currentDay < futureDay)){
    Serial.println("Tuleva päivämäärä on myöhemmin kuin nykyinen päivämäärä");
} else if(currentYear == futureYear && currentMonth == futureMonth && currentDay == futureDay) {
    Serial.println("Päivämäärät ovat samat");
} else {
    Serial.println("Tuleva päivämäärä on ennen nykyistä päivämäärää");
}
```

Koodi tulostaa ensin nykyisen päivämäärän ja sitten vertailee sitä tulevaan päivämäärään. Kuten huomataan, vertailun logiikka on hieman monimutkaisempi, mutta hyödyntämällä Arduino-funktioita voimme tehdä vertailusta helpompaa.

## Katso myös
-