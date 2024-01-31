---
title:                "Säännöllisten lausekkeiden käyttö"
date:                  2024-01-19
html_title:           "Arduino: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"

category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? - Mitä & Miksi?
Regular expressions eli säännölliset lausekkeet ovat kaavoja tekstin etsimiseen ja käsittelyyn. Niiden avulla voimme tunnistaa, varmentaa ja jäsentää monimutkaisia tekstimuotoja tehokkaasti.

## How to: - Näin teet:
```Arduino
// Ei natiivia RegExp-tukea, mutta voit käyttää funktioita ja kirjastoja:
#include <regex.h>

void setup() {
  Serial.begin(9600);
  // Oletetaan, että haluat tunnistaa kelvolliset IP-osoitteet:
  const char *ipPattern = R"(^\d{1,3}(\.\d{1,3}){3}$)";
  regex_t regex;
  
  // Alusta regular expression:
  if (regcomp(&regex, ipPattern, REG_EXTENDED) == 0) {
    Serial.println("RegEx alustettu onnistuneesti");
  } else {
    Serial.println("Virhe alustettaessa RegEx");
  }
  
  // Tarkista vastaavuus:
  const char *testIp = "192.168.1.1";
  if (regexec(&regex, testIp, 0, NULL, 0) == 0) {
    Serial.println("IP-osoite on validi");
  } else {
    Serial.println("IP-osoite ei ole validi");
  }
  
  // Vapauta resurssit:
  regfree(&regex);
}

void loop() {
  // Tässä ei tarvita loopin toiminnallisuutta.
}
```
Huomaa, että Arduino ei suoraan tue säännöllisiä lausekkeita, joten esimerkissä käytetään `regex.h`-kirjastoa.

## Deep Dive - Syväsukellus
Perinteisesti Arduino ei sisällä regular expressions -ominaisuutta sen pienentyneen muistitilan ja prosessoritehon takia. Muistiintehokkaampia menetelmiä, kuten merkkijonojen haku ja korvaus, on usein käytetty. Uudempien tai enemmän resursseja omaavien laitteistojen, kuten ESP8266:n kanssa, voit käyttää Regex-kirjastoja tarkempiin tekstianalyyseihin.

## See Also - Katso Myös
- `regex.h` dokumentaatio: https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/regex.h.html
- Regular expressions -perusteet: https://www.regular-expressions.info/
- Stack Overflow -keskustelut ja esimerkit: https://stackoverflow.com/search?q=arduino+regex
