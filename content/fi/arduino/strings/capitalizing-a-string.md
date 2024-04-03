---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:17.214058-07:00
description: "Miten: Arduino, joka on p\xE4\xE4asiassa tunnettu vuorovaikutuksesta\
  \ laitteiston kanssa, sis\xE4lt\xE4\xE4 my\xF6s perustason merkkijonon k\xE4sittelykykyj\xE4\
  \u2026"
lastmod: '2024-03-13T22:44:56.808542-06:00'
model: gpt-4-0125-preview
summary: "Arduino, joka on p\xE4\xE4asiassa tunnettu vuorovaikutuksesta laitteiston\
  \ kanssa, sis\xE4lt\xE4\xE4 my\xF6s perustason merkkijonon k\xE4sittelykykyj\xE4\
  \ `String`-objektinsa kautta."
title: Merkkijonon muuttaminen isoiksi kirjaimiksi
weight: 2
---

## Miten:
Arduino, joka on pääasiassa tunnettu vuorovaikutuksesta laitteiston kanssa, sisältää myös perustason merkkijonon käsittelykykyjä `String`-objektinsa kautta. Kuitenkaan siinä ei ole suoraa `capitalize`-funktiota, joka nähdään korkeamman tason kielissä. Näin ollen toteutamme suurkirjoittamisen iteroimalla merkkijonon yli ja soveltamalla kirjainkoon muutoksia.

Tässä on perusesimerkki ilman kolmannen osapuolen kirjastoja:

```cpp
String capitalizeString(String input) {
  if (input.length() == 0) {
    return ""; // Palauta tyhjä merkkijono, jos syöte on tyhjä
  }
  input.toLowerCase(); // Muunna koko merkkijono ensin pienaakkosiksi
  input.setCharAt(0, input.charAt(0) - 32); // Säädä ensimmäinen merkki suuraakkoseksi
  
  // Säädä suuraakkoseksi kirjaimet, jotka seuraavat välilyöntiä
  for (int i = 1; i < input.length(); i++) {
    if (input.charAt(i - 1) == ' ') {
      input.setCharAt(i, input.charAt(i) - 32);
    }
  }
  return input;
}

void setup() {
  Serial.begin(9600);
  String testStr = "hello arduino world";
  String capitalizedStr = capitalizeString(testStr);
  Serial.println(capitalizedStr); // Tuloste: "Hello Arduino World"
}

void loop() {
  // Tyhjä silmukka
}
```

Tämä koodipätkä määrittelee `capitalizeString`-funktion, joka ensin muuntaa koko merkkijonon pienaakkosiksi standardoidakseen sen kirjainkoon. Sitten se suurkirjoittaa ensimmäisen merkin ja minkä tahansa välilyönnin jälkeen tulevan merkin, tehokkaasti suurkirjoittaen jokaisen sanan syötteen merkkijonossa. Huomaa, että tämä alkeellinen toteutus olettaa ASCII-merkistökoodauksen ja saattaa vaatia säätöjä täyden Unicode-tuen saavuttamiseksi.

Tällä hetkellä ei ole laajasti hyväksyttyjä kolmannen osapuolen kirjastoja nimenomaan merkkijonon käsittelyyn Arduinon ekosysteemissä, pääasiassa sen keskittyessä laitteistoon vuorovaikutukseen ja tehokkuuteen. Kuitenkin tarjottu esimerkki on suoraviivainen tapa saavuttaa merkkijonon suurkirjoittaminen Arduinon ohjelmointiympäristössä.
