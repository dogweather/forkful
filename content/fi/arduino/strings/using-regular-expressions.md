---
aliases:
- /fi/arduino/using-regular-expressions/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:22.633854-07:00
description: "S\xE4\xE4nn\xF6lliset lausekkeet (regex) ovat merkkijonoja, jotka m\xE4\
  \xE4rittelev\xE4t hakukuviot ja joita k\xE4ytet\xE4\xE4n ensisijaisesti merkkijonojen\
  \ haussa ja\u2026"
lastmod: 2024-02-18 23:09:07.889372
model: gpt-4-0125-preview
summary: "S\xE4\xE4nn\xF6lliset lausekkeet (regex) ovat merkkijonoja, jotka m\xE4\xE4\
  rittelev\xE4t hakukuviot ja joita k\xE4ytet\xE4\xE4n ensisijaisesti merkkijonojen\
  \ haussa ja\u2026"
title: "S\xE4\xE4nn\xF6llisten lausekkeiden k\xE4ytt\xF6"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Säännölliset lausekkeet (regex) ovat merkkijonoja, jotka määrittelevät hakukuviot ja joita käytetään ensisijaisesti merkkijonojen haussa ja manipuloinnissa. Ohjelmoijat hyödyntävät regexiä Arduino-projekteissa sarjasyötteiden jäsentämiseen, käyttäjän syötteen validointiin tai tietojen poimintaan merkkijonoista, mikä tehostaa datan käsittelyn tehokkuutta ja joustavuutta.

## Miten:
Arduino ei suoraan tue regexiä sen vakio-kirjastossa. Voit kuitenkin saavuttaa regexiä muistuttavaa toiminnallisuutta yksinkertaisille kuviolle käyttämällä perusmerkkijonofunktioita, tai monimutkaisempiin tarpeisiin integroimalla kolmannen osapuolen kirjaston, kuten `regex`.

### Yksinkertainen merkkijonohaku ilman Regexiä
Perustarpeisiin, kuten alimerkkijonon löytämiseen, voit käyttää `String.indexOf()`-funktiota:
```cpp
String data = "Sensor arvo: 12345";
int index = data.indexOf("arvo:");
if (index != -1) {
  String value = data.substring(index + 6).trim();
  Serial.println(value); // Tulostaa: 12345
}
```

### Kolmannen osapuolen kirjaston käyttö Regexille
Monimutkaisempien kuvioiden käsittelyyn saatat harkita kirjastoa, kuten `regex`. Kirjaston asentamisen jälkeen voit käyttää sitä seuraavasti:

1. **Asennus**: `regex`-kirjastoa ei ehkä ole suoraan saatavilla Arduino-kirjastohallinnassa, joten saatat joutua asentamaan sen manuaalisesti lataamalla luotettavasta lähteestä ja lisäämällä sen Arduinon kirjastoihisi.

2. **Esimerkkikäyttö**:
Olettaen, että kirjasto tarjoaa toiminnallisuuksia vastaavasti kuin standardit regex-toteutukset, saatat käyttää sitä seuraavasti:

```cpp
#include <regex.h>

void setup() {
  Serial.begin(9600);
  while (!Serial); // Odota, että Serial on valmis
  
  regex_t reg;
  const char* pattern = "[0-9]+"; // Vastaa numerosarjaa
  regcomp(&reg, pattern, REG_EXTENDED);
  
  const char* test_str = "Sensor arvo: 12345";
  
  regmatch_t matches[1];
  if (regexec(&reg, test_str, 1, matches, 0) == 0) {
    // Poimi ja tulosta vastaava osa
    int start = matches[0].rm_so;
    int end = matches[0].rm_eo;
    char match[end-start+1];
    strncpy(match, test_str + start, end-start);
    match[end-start] = '\0';
    
    Serial.print("Löydetty vastaavuus: ");
    Serial.println(match); // Tulostaa: 12345
  } else {
    Serial.println("Vastaavuutta ei löydy");
  }
  
  regfree(&reg); // Vapauta regexille varattu muisti
}

void loop() {
  // laita pääkoodisi tähän, suoritetaan toistuvasti:
}
```

**Huom**: Tässä käytetty syntaksi ja tiettyjen funktioiden käyttö on esimerkinomaisia ja saattavat vaihdella valitsemasi `regex`-kirjaston todellisen toteutuksen yksityiskohtien mukaan. Viittaa aina kirjaston dokumentaatioon saadaksesi tarkkaa ja ajan tasalla olevaa tietoa.
