---
title:                "Kahden päivämäärän vertaaminen"
html_title:           "Bash: Kahden päivämäärän vertaaminen"
simple_title:         "Kahden päivämäärän vertaaminen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Mikä ja Miksi?

Päivämäärien vertailu on prosessi, jossa kahden kalenteripäivän välinen suhde määritellään. Ohjelmoijat tekevät tämän, kun heidän täytyy tarkistaa tapahtumien suhteellinen esiintymisjärjestys tai määrittää aikajakso kahden tapahtuman välillä.

## Kuinka tehdä:

```Arduino
// Luo DateTime-olioita käyttämällä vuotta, kuukautta, päivää, tuntia, minuuttia ja sekuntia
DateTime dt1(2021, 7, 19, 12, 30, 45);
DateTime dt2(2021, 7, 20, 8, 15, 0);

// Vertaile kahta päivämäärää
if(dt1 < dt2){
  Serial.println("dt1 on ennen dt2");  // Tulostaa, jos dt1 on ennen dt2 
}else if(dt1 > dt2){
  Serial.println("dt1 on jälkeen dt2");  // Tulostaa, jos dt1 on jälkeen dt2 
}else{
  Serial.println("dt1 ja dt2 ovat samat");  // Tulostaa, jos dt1 ja dt2 ovat samat
}
```

## Syvällisempi syöksy

Ennen Arduinon päivämäärä- ja aikakirjastoa, päivämäärien vertailu saattoi olla haastavaa. Tämä johtuu siitä, että monimutkaisten ajanhallintasäännösten, kuten karkausvuosien ja kesäajan, hallinta ei ole yksinkertainen tehtävä.

Vaihtoehtoisesti voit käyttää millis() ja micros() -toimintoja, mutta ne ovat tehokkaita vain, jos haluat seurata aikaa suhteellisesti, ei absoluuttisesti.

Päivämäärien vertailussa DateTime-luokan ylikuormitettua ”<”, ”>” ja ”==” operaattoria hyödynnetään. Nämä operaattorit tekevät vertailun dt1: n ja dt2: n välillä suoraan käytettäessä.

## Katso myös:

1. Arduinon virallinen aika-kirjasto: [Arduino Time Library](https://www.arduino.cc/reference/en/libraries/time/)
2. Ajanhallinta Arduino-ohjelmoinnissa: [Arduino Time Management](https://learn.adafruit.com/multi-tasking-the-arduino-part-1/time-management)
3. Lisätietoja DateTime-luokasta: [Arduino DateTime Class](https://www.arduino.cc/reference/en/libraries/datetime/)