---
title:    "Arduino: Merkkijonon muuttaminen isokyseksi"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Miksi sinun kannattaisi käyttää Arduinoa ja koodata merkkijonon kirjaimet isolla?

Ehkä haluat tehdä projektin, jossa haluat korostaa tiettyjä sanoja tai nimiä, tai ehkä haluat vain vaikuttaa hienostuneelta. Joka tapauksessa, merkkijonon kirjainten muuttaminen isoksi on hyödyllinen taito kaikille Arduino-ohjelmoijille.

## Kuinka

Koodiesimerkkien avulla tässä selitetään kuinka voit helposti muuttaa merkkijonon kaikki kirjaimet isoksi käyttämällä Arduinoa.

```Arduino
String teksti = "Hei maailma";
String uusi_teksti = teksti.toUpperCase();
Serial.println(uusi_teksti);
```
Tässä esimerkissä merkkijono "Hei maailma" muutetaan isoksi ja tulostetaan sarjaportin kautta. Tulostus näyttää seuraavalta: "HEI MAAILMA". Huomaa, että alkuperäinen merkkijono ei muutu, vaan uusi muutettu merkkijono tallennetaan uuteen muuttujaan.

Voit myös käyttää tätä toimintoa yhdessä `if`-lausekkeen kanssa tarkistaaksesi, sisältääkö merkkijono tiettyjä kirjaimia. Esimerkiksi:

```Arduino
String salasana = "Salainen";
String syote = Serial.readStringUntil('\n'); //Luetaan käyttäjän antama syöte
if (syote.toUpperCase() == salasana) { //Muutetaan käyttäjän syöte isoksi ja verrataan siihen salasanaa
  Serial.println("Oikea salasana!");
} else {
  Serial.println("Väärä salasana!");
}
```
Tässä esimerkissä käytetään `toUpperCase()`-funktiota, jotta käyttäjän syöte voidaan verrata salasanaan riippumatta siitä, kirjoittaako käyttäjä sen isolla vai pienellä alkukirjaimella.

## Syvällisempi tutustuminen

Arduino `String`-luokka tarjoaa kätevän `toUpperCase()`-funktion, joka muuttaa merkkijonon kaikki kirjaimet isoksi käyttämällä Unicode-taulukkoa.

Koodin tarkastelussa voit huomata, että `toUpperCase()`-funktio käyttää itse asiassa `toUpper()`-funktiota, joka on määritelty Unicode-merkkien käsittelyssä.

Vaikka `toUpperCase()`-funktio on käytännöllinen ja helppo tapa muuttaa merkkijonon kirjaimet isoksi, on myös tärkeää pitää mielessä, että se muuttaa vain kirjaimet, joilla on pieni alkukirjain. Jos merkkijonossa on esimerkiksi erikoismerkkejä tai numeroita, ne eivät muutu isoksi.

## Katso myös
- [Arduino String-luokka](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [Unicode-merkkien käsittely](https://www.unicode.org/)