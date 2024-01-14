---
title:    "Arduino: Uuden projektin aloittaminen"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Miksi

Oletko koskaan katsellut Arduino-projekteja verkossa ja ajatellut, että haluat kokeilla tehdä jotain samanlaista? Ehkä sinulla on jo ideoita, mutta et tiedä mistä aloittaa. Tässä blogikirjoituksessa jaamme joitain perusohjeita siitä, miten aloitat uuden Arduino-projektin.

## Kuinka aloittaa 

Aloittaminen uuden Arduino-projektin kanssa voi tuntua hieman pelottavalta, mutta se on helpompaa kuin luuletkaan. Tässä on muutama askel, joita voit seurata saadaksesi projektisi alkuun: 

1. Ennen kuin aloitat projektin, tutustu Arduinon perusteisiin ja varmista, että sinulla on tarvittavat laitteet ja komponentit.
2. Aloita asentamalla Arduino IDE tietokoneellesi. Tämä on ohjelmisto, jota käytetään Arduinon ohjelmointiin ja jolla voit ladata koodin Arduino-piirilevylle.
3. Liitä Arduino-piirilevy tietokoneeseesi USB-kaapelilla ja avaa Arduino IDE.
4. Valitse IDE:stä oikea asetus tietokoneesi ja Arduino-piirilevysi välillä.
5. Kirjoita koodisi IDE:hen ```Arduino...``` -lohkoon. Esimerkiksi voit kokeilla seuraavaa yksinkertaista koodia, joka saa LED-valon vilkkumaan:

```
Arduino void setup () {
  pinMode (LED_BUILTIN, OUTPUT);
}

void loop () {
  digitalWrite (LED_BUILTIN, HIGH);
  delay (1000);
  digitalWrite (LED_BUILTIN, LOW) ;
  delay (1000);
}
```

6. Klikkaa "Upload" -painiketta ladataksesi koodin Arduino-piirilevyysi.
7. Näet nyt, kuinka LED-valo vilkkuu yhteen tahtiin sekunnin välein.

Tämä oli vain yksinkertainen esimerkki, mutta voit nyt alkaa kokeilla muita toimintoja ja komponentteja sekä kirjoittaa omaa koodia.

## Syvemmälle aiheeseen

Aloittaminen Arduinon kanssa on vain ensimmäinen askel. Kokeile rohkeasti erilaisia projekteja ja haasta itseäsi oppimaan uusia asioita. Muista myös, että internetistä löytyy runsaasti apua, jos takerrut johonkin ongelmaan. Arduinoyhteisö on suuri ja ystävällinen ja sieltä voi saada paljon neuvoja ja inspiraatiota uusiin projekteihin.

## Katso myös

- [Arduinon kotisivut](https://www.arduino.cc/) 
- [Arduinon oppimateriaalit](https://www.arduino.cc/en/Tutorial/HomePage)
- [Arduinoyhteisö](https://forum.arduino.cc/)