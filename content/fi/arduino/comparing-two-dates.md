---
title:                "Arduino: Kahden päivämäärän vertailu"
simple_title:         "Kahden päivämäärän vertailu"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Miksi: Miksi vertailla kahden päivämäärän välillä?

Päivämäärän vertailu on tärkeää etenkin projektien aikataulutuksessa ja tapahtumien järjestämisessä. Tämä on myös hyödyllistä, jos haluat tarkistaa, onko jokin päivämäärä jo mennyt tai onko se tulevaisuudessa.

## Miten: Esimerkkejä Arduino-ohjelmoinnista vertaillaksesi kahden päivämäärän välillä

```Arduino
#include <Time.h>

int syotePv = 25; //ensimmäisen päivämäärän päivä
int syoteKk = 11; //ensimmäisen päivämäärän kuukausi
int syoteV = 2021; //ensimmäisen päivämäärän vuosi

int tulosPv = 10; //toisen päivämäärän päivä
int tulosKk = 8; //toisen päivämäärän kuukausi
int tulosV = 2022; //toisen päivämäärän vuosi

// Luo aikatietueet ensimmäiselle ja toiselle päivämäärälle
tmElements_t ensimmainen = {0, 0, 0, syotePv, syoteKk, syoteV + CalendarYrOffset};
tmElements_t toinen = {0, 0, 0, tulosPv, tulosKk, tulosV + CalendarYrOffset};

//Muunna tietueet ajan luvuiksi
time_t aika1 = makeTime(ensimmainen);
time_t aika2 = makeTime(toinen);

//Vertaile aikoja
if(aika1 < aika2){
  Serial.println("Ensimmäinen päivämäärä on ennen toista päivämäärää.");
}
else if(aika1 == aika2){
  Serial.println("Päivämäärät ovat samat.");
}
else{
  Serial.println("Ensimmäinen päivämäärä on jälkeen toista päivämäärää.");
}
```

Esimerkki tulostaa "Ensimmäinen päivämäärä on ennen toista päivämäärää.", koska ensimmäinen päivämäärä on vuonna 2021 ja toinen päivämäärä vuonna 2022.

## Syvemmälle: Lisätietoa kahden päivämäärän vertailusta

Päivämäärien vertailu toimii käyttämällä aikatietueita, jotka muunnetaan aikaluvuiksi ja sitten verrataan keskenään. Arduino-koodissa tämä tapahtuu käyttämällä Time-kirjastoa, joka sisältää hyödyllisiä toimintoja aikatietueiden käsittelyyn.

Vertailussa on tärkeää ottaa huomioon myös vuodet, jotta tulokset ovat oikein. Tästä syystä tietueet on muunnettava aikatietueiksi käyttämällä makeTime-funktiota.

## Katso myös

- [Time Library for Arduino](https://www.arduino.cc/en/reference/time)
- [Arduino Reference - Time functions](https://www.arduino.cc/reference/en/libraries/time/)
- [YouTube: Arduino Time and Date Tutorials Playlist](https://www.youtube.com/playlist?list=PLA567CE235D39FA84)