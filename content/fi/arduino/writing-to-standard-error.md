---
title:    "Arduino: Kirjoittaminen standardivirheen kautta"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

# Miksi kirjoittaa standardi virheeseen

Kirjoittaessa Arduino-koodia, saatat törmätä tilanteisiin, joissa haluat ilmoittaa virheistä tai tärkeistä tiedoista käyttäjälle ohjelman suorituksen aikana. Yksi tapa tehdä tämä on kirjoittamalla tietoa standardi virheeseen. Tässä blogikirjoituksessa käymme läpi, mitä se tarkoittaa ja miten se tehdään.

## Kuinka tehdä se

Käytä ```Arduino Serial.print()```-funktiota lähettääksesi tietoa standardi virheeseen. Voit käyttää tätä esimerkiksi ilmoittaaksesi, jos jokin anturi ei toimi oikein tai jos jokin tapahtuma ei onnistu. Esimerkiksi:

```Arduino
Serial.print("Virhe havaittu!"); 
```

Tämä lähettää viestin "Virhe havaittu!" standardi virheeseen ja näet sen sarjaportin monitorissa, jos sitä käytetään.

Voit myös määrittää, mihin sarjaportin monitorissa näytettyyn rivin loppuun lisätään rivinvaihto käyttämällä ```Arduino Serial.println()```-funktiota. Tämä voi helpottaa viestien erottelua toisistaan. Esimerkiksi:

```Arduino
Serial.println("Tämä näkyy omalla rivillään.\nTämä tulee seuraavalle riville.");
```

## Syvällinen sukellus

Standardi virhe on tapa lähettää tietoa ohjelman suorituksen aikana, mutta miksi pitäisi käyttää sitä eikä vain tulostaa tietoa sarjaporttiin? Yksi syy on, että virheitä saattaa olla vaikeampi havaita, jos ne sekoittuvat muun sarjaporttiin tulostetun tiedon joukkoon. Käyttämällä standardi virhettä, keskitytään vain virheisiin ja ne erottuvat selkeästi muusta tiedosta.

Toinen etu on, että ```Arduino Serial.print()```-funktiota voidaan käyttää myös vaihtoehtona sarjaportin monitorille. Voit esimerkiksi lähettää tiedon standardi virheeseen ja sitten ohjata sen näkymään tietokoneen konsolissa.

## Katso myös

- [Arduino Serial kommunikointi](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [Arduinon sarjaportin monitori](https://www.arduino.cc/en/Serial/Monitor)
- [Arduino dokumentaatio suomeksi](https://www.arduino.cc/reference/en)