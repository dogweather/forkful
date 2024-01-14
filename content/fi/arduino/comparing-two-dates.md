---
title:    "Arduino: Vertailla kahden päivämäärän välillä"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Miksi vertailla kahta päivämäärää?

On monia tilanteita, joissa Arduino-ohjelmoijan on vertailtava kahta päivämäärää. Tämä voi olla hyödyllistä esimerkiksi, kun halutaan tarkistaa, onko jokin tapahtuma tulevaisuudessa vai menneisyydessä, tai jos halutaan laskea kuinka monta päivää on kulunut tietystä tapahtumasta. Tutustukaamme siis siihen, miten Arduino-ohjelmassa voidaan vertailla kahta päivämäärää.

## Miten suorittaa vertailu?

Vertailemiseen käytetään C++:n tarjoamia perusfunktioita, kuten "if" ja "else". Alla olevassa esimerkissä vertaillaan kahta päivämäärää: 1. tammikuuta 2021 ja 15. joulukuuta 2020.

```Arduino
void setup() {
  Serial.begin(9600); // Alustetaan sarjaportti
  int paiva = 1, kuukausi = 1, vuosi = 2021; // Määritetään ensimmäinen päivämäärä
  int vertailu_paiva = 15, vertailu_kuukausi = 12, vertailu_vuosi = 2020; // Määritetään vertailtava päivämäärä

  if (paiva == vertailu_paiva && kuukausi == vertailu_kuukausi && vuosi == vertailu_vuosi) { 
  // Vertaillaan päivä-, kuukausi- ja vuosilukuja
    Serial.println("Päivämäärät ovat samat!"); // Tulostetaan viesti, jos päivämäärät ovat samat
  } else {
    Serial.println("Päivämäärät ovat erilaiset!"); // Tulostetaan viesti, jos päivämäärät ovat erilaiset
  }
}

void loop() {
  // Tyhjä silmukka, ei toimintaa
}
```

Tulostus:
```
Päivämäärät ovat erilaiset!
```

Tässä esimerkissä verrataan päivämääriä vain tarkistamalla, ovatko kaikki kolme tietoa (päivä, kuukausi ja vuosi) samat. Voit kuitenkin myös vertailla vain yhtä tai kahta tietoa, riippuen siitä, mitä haluat saavuttaa.

## Syvällinen sukellus

Kun vertailet kahta päivämäärää, sinun on otettava huomioon erilaisia asioita, kuten kuukausien eri pituudet ja karkausvuodet. Voit käyttää esimerkiksi "if" -lauseiden sarjaa, jossa vertaillaan ensin vuosia, sitten kuukausia ja lopuksi päiviä. Lisäksi voit käyttää valmiita funktioita, kuten "daysInMonth()", joka palauttaa kyseisen kuukauden päivien määrän.

Lisäksi tarkkuuden varmistamiseksi on hyvä käyttää ISO 8601 -standardin mukaista päivämäärämuotoa, joka on vuosi-kuukausi-päivä (esimerkiksi 2021-01-01). Tällä tavoin päivämäärien vertaileminen on helpompaa ja vähemmän virhealtista.

## Katso myös

- [ISO 8601 -standardi](https://fi.wikipedia.org/wiki/ISO_8601)
- [Arduino Reference - if](https://www.arduino.cc/reference/en/language/structure/control-structure/if/)
- [Arduino Reference - daysInMonth()](https://www.arduino.cc/reference/en/language/functions/time/daysinmonth/)