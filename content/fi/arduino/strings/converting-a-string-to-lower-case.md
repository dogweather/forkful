---
title:                "Merkkijonon muuntaminen pieniksi kirjaimiksi"
aliases:
- /fi/arduino/converting-a-string-to-lower-case/
date:                  2024-01-20T17:37:52.192906-07:00
model:                 gpt-4-1106-preview
simple_title:         "Merkkijonon muuntaminen pieniksi kirjaimiksi"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Muuttaminen merkkijonosta pieniksi kirjaimiksi muuntaa kaikki kirjaimet pieniksi. Tämä auttaa vertailemaan ja käsittelemään merkkijonoja tapausriippumattomasti.

## Kuinka tehdä:
Arduino-koodissa merkkijonon muuttaminen pieniksi kirjaimiksi on suoraviivaista. `String`-luokalla on `toLowerCase()`-metodi. Tässä on esimerkki:

```arduino
String original = "Hyvää päivää, Maailma!";
original.toLowerCase();
Serial.println(original);  // tulostaa: "hyvää päivää, maailma!"
```
Tässä on toinen, kun käytät `char`-taulukkoa:

```arduino
void setup() {
  Serial.begin(9600);
}

void loop() {
  char teksti[] = "Moi Taas, MAAILMA!";
  lowerCaseConverter(teksti);
  Serial.println(teksti);  // tulostaa: "moi taas, maailma!"
  delay(2000);  // Odota 2 sekuntia ennen seuraavaa tulostusta
}

void lowerCaseConverter(char* input) {
  for ( ; *input; ++input) *input = toLowerCase(*input);
}

char toLowerCase(char c) {
  if (c >= 'A' && c <= 'Z') {
    return c + 32;
  } else {
    return c;
  }
}
```

## Syväsukellus
Historiallisesti merkkijonojen käsittely, kuten pieniksi kirjaimiksi muuttaminen, juontaa juurensa varhaisista tietokonejärjestelmistä ja tarpeesta standardoida tekstiä. Vaihtoehtoisia menetelmiä ovat `tolower()` C-kirjastossa tai jopa manuaaliset taulukkotarkistukset. Arduino toteuttaa `String`-luokassa `toLowerCase()`, joka kätkee toteutusyksityiskohdat, kuten ASCII-arvojen käsittelyn.

## Katso myös
- Arduinon ohjeet String-luokasta: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- C++ tolower-funktio: http://www.cplusplus.com/reference/cctype/tolower/
