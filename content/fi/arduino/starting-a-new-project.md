---
title:                "Aloittamassa uutta projektia"
html_title:           "Arduino: Aloittamassa uutta projektia"
simple_title:         "Aloittamassa uutta projektia"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Miksi

Miksi aloittaa uusi projekti Arduinoa käyttäen? Loppujen lopuksi on olemassa monia muitakin tapoja toteuttaa elektroniikkaprojekteja. Mutta miksi Arduino on yleensäkin vaihtoehto, ja mitä hyötyä siitä on?

Arduino on helppo käyttää ja monipuolinen alusta, jolla voi toteuttaa lähes mitä tahansa elektroniikkaprojektia. Se tarjoaa myös runsaasti ohjeita ja tutoriaaleja eri alojen projekteihin, joten aloittaminen on helppoa vaikka et olisikaan elektroniikan asiantuntija.

## Näin aloitat uuden projektin

Aloittaaksesi uuden projektin Arduinolla, sinun tarvitsee ensin hankkia tarvittavat välineet. Perusvaatimuksia ovat Arduino-mikrokontrolleri, piirilevy, johdot, vastukset ja tarvittaessa erilaisia antureita ja moduuleita.

Sitten tarvitset myös Arduino-ohjelmiston, jonka voit ladata ilmaiseksi Arduino-verkkosivuilta. Ohjelmisto mahdollistaa koodin kirjoittamisen, yhdistämisen mikrokontrolleriin ja testaamisen.

Valitse sitten projektillesi tarkoituksenmukainen ohjelmointikieli ja aloita koodin kirjoittaminen. Esimerkiksi seuraava koodinpätkä saa LEDin vilkkumaan kerran sekunnissa:

```Arduino
int ledPin = 13; // määritetään LEDin pinni
void setup() {
  pinMode(ledPin, OUTPUT); // LEDin pin-lähtötila asetetaan lähdöksi
}
void loop() {
  digitalWrite(ledPin, HIGH); // LEDiä sytytetään
  delay(1000);                // odotetaan yksi sekunti
  digitalWrite(ledPin, LOW);  // LED sammutetaan
  delay(1000);                // odotetaan taas
}
```

Kun koodi on valmis, voit ladata sen Arduino-mikrokontrolleriin ja tarkistaa, että projekti toimii halutulla tavalla.

## Syvemmälle projektin aloittamiseen

Kun olet päässyt vauhtiin Arduinon käytössä, voit tutustua esimerkiksi erilaisiin laajennuksiin ja biblioteekkeihin, jotka tarjoavat valmiita toimintoja ja helpottavat koodin kirjoittamista. Voit myös tutkia erilaisia antureita ja moduuleita, joilla voit lisätä monipuolisuutta projektiisi.

Toinen tapa syventyä Arduinon käyttöön on seurata Arduino-yhteisön keskusteluita ja osallistua haasteisiin ja kilpailuihin, joita järjestetään maailmanlaajuisesti. Näin voit oppia uutta ja saada inspiraatiota uusiin projekteihin.

## Katso myös

- Arduino-verkkosivusto: https://www.arduino.cc/
- Arduino-projekteja ja tutoriaaleja: https://create.arduino.cc/projecthub
- Arduino-yhteisöfoorumi: https://forum.arduino.cc/