---
title:    "Arduino: Testien kirjoittaminen"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Miksi
On monia syitä, miksi kirjoitat testejä Arduino-ohjelmille. Näihin kuuluvat mm. koodin virheiden löytäminen ja korjaaminen, uusien ominaisuuksien integroiminen ja varmistaminen, että ohjelma toimii tarkoitetussa laitteessa ja ympäristössä.

## Kuinka
Testien kirjoittaminen Arduino-ohjelmille on helppoa. Aluksi sinun tulee määrittää testauksen kohteena oleva funktio tai toiminto. Tämän jälkeen sinun tulee luoda testitapauksia, joissa määritellään odotettu tulostus erilaisille syötteille. Lopuksi voit kirjoittaa koodin, joka ajaa testit ja tulostaa tulokset.

```Arduino
// Alkuperäinen funktio
int summa(int x, int y) {
  return x + y;
}

// Testien alustus
void setup() {
  // aseta sarjaportti käyttöön tulosten tulostamista varten
  Serial.begin(9600);
}

// Yksittäinen testitapaus
void testi1() {
  int tulos = summa(2, 3); // kutsu alkuperäistä funktiota
  // tarkista, onko tulos odotettu 5
  if (tulos == 5) {
    Serial.println("Testitapaus 1: läpäisty"); // tulosta tulos sarjaportille
  } else {
    Serial.println("Testitapaus 1: epäonnistunut");
  }
}

// Yksittäinen testitapaus
void testi2() {
  int tulos = summa(2, -3); // kutsu alkuperäistä funktiota
  // tarkista, onko tulos odotettu -1
  if (tulos == -1) {
    Serial.println("Testitapaus 2: läpäisty"); // tulosta tulos sarjaportille
  } else {
    Serial.println("Testitapaus 2: epäonnistunut");
  }
}

void loop() {
  // kutsu testitapaukset loop-funktion sisällä
  testi1();
  testi2();
}

```

```
Sarjaportilta tulostuu seuraava tulos:
Testitapaus 1: läpäisty
Testitapaus 2: läpäisty
```

## Syväsukellus
Testien avulla voit myös tarkastella ja varmistaa ohjelman rakennetta ja toimintaa. Esimerkiksi voit kirjoittaa testejä eri toiminnoille ja luokille, mikä auttaa hahmottamaan kokonaisuutta ja löytämään mahdollisia ongelmia.

Lisäksi testien avulla voit simuloida ja testata erilaisia olosuhteita ja reaktioita, joita ohjelmasi joutuu kohtaamaan käytön aikana. Tämä auttaa löytämään ja korjaamaan bugeja sekä varmistamaan, että ohjelma toimii mahdollisimman virheettömästi ja luotettavasti.

## Katso myös
- [Arduino - Test Driven Development](https://www.arduino.cc/en/pmwiki.php?id=guide/EnvironmentTestDrivenDevelopment)
- [How Debugging works on Arduino](https://www.arduino.cc/en/Guide/EnvironmentDebugging)
- [Testing and Debugging Arduino Programs](https://www.arduino.cc/en/Tutorial/BuiltInExamples/TestingAndDebugging)