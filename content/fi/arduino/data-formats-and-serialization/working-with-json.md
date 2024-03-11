---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:54.106634-07:00
description: "JSON eli JavaScript Object Notation on kevyt tiedonvaihtoformaatti,\
  \ joka sopii erinomaisesti sek\xE4 tietojen tallennukseen ett\xE4 kokoonpanotiedostoihin\u2026"
lastmod: '2024-03-11T00:14:30.873044-06:00'
model: gpt-4-0125-preview
summary: "JSON eli JavaScript Object Notation on kevyt tiedonvaihtoformaatti, joka\
  \ sopii erinomaisesti sek\xE4 tietojen tallennukseen ett\xE4 kokoonpanotiedostoihin\u2026"
title: "Ty\xF6skentely JSON:n kanssa"
---

{{< edit_this_page >}}

## Mikä & Miksi?

JSON eli JavaScript Object Notation on kevyt tiedonvaihtoformaatti, joka sopii erinomaisesti sekä tietojen tallennukseen että kokoonpanotiedostoihin Arduino-projekteissa. Ohjelmoijat käyttävät sitä sen yksinkertaisuuden ja luettavuuden vuoksi eri ohjelmointiympäristöissä, myös Arduinossa, mikä mahdollistaa saumattoman tiedonvaihdon verkkopalveluiden API-rajapintojen tai muiden järjestelmien kanssa.

## Kuinka:

JSONin käyttämiseen Arduinossa suosittu valinta on `ArduinoJson`-kirjasto sen helppokäyttöisyyden ja tehokkuuden vuoksi. Se mahdollistaa JSON-merkkijonojen jäsentämisen (parsimisen), niiden muokkaamisen ja olioiden serialisoinnin takaisin JSON-merkkijonoiksi. Näin se tapahtuu:

1. **Asenna ArduinoJson-kirjasto**: Käytä kirjastohallintaa Arduino IDE:ssä ja asenna "ArduinoJson".

2. **Deserialisoi JSON-merkkijono**: Tässä on tapa jäsentää JSON-merkkijono ja poimia arvot.

```cpp
#include <ArduinoJson.h>

const char* json = "{\"sensor\":\"gps\",\"time\":1351824120,\"data\":[48.756080,2.302038]}";

void setup() {
  Serial.begin(9600);
  StaticJsonDocument<200> doc; // Säädä koko JSON-dokumentin mukaan
  DeserializationError error = deserializeJson(doc, json);

  if (error) {
    Serial.print(F("deserializeJson() epäonnistui: "));
    Serial.println(error.f_str());
    return;
  }

  const char* sensor = doc["sensor"]; // "gps"
  long time = doc["time"]; // 1351824120
  float latitude = doc["data"][0]; // 48.756080
  float longitude = doc["data"][1]; // 2.302038
  
  Serial.println(sensor);
  Serial.println(time);
  Serial.println(latitude, 6);
  Serial.println(longitude, 6);
}

void loop() {
  // Tyhjä silmukka
}
```

Esimerkkituloste:

```
gps
1351824120
48.756080
2.302038
```

3. **Serialisoi JSON-merkkijonoksi**: Tässä on tapa luoda tietoja sisältävä JSON-merkkijono.

```cpp
#include <ArduinoJson.h>

void setup() {
  Serial.begin(9600);

  StaticJsonDocument<200> doc; // Säädä koko datan mukaan
  doc["sensor"] = "gps";
  doc["time"] = 1351824120;
  JsonArray data = doc.createNestedArray("data");
  data.add(48.756080);
  data.add(2.302038);

  serializeJson(doc, Serial);
}

void loop() {
  // Tyhjä silmukka
}
```

Esimerkkituloste (lueteltu selkeyden vuoksi):

```
{"sensor":"gps","time":1351824120,"data":[48.756080,2.302038]}
```

`ArduinoJson`-kirjaston tehokas käyttö mahdollistaa monimutkaisten tietorakenteiden viestinnän ihmisen luettavassa muodossa Arduino-projekteissa, helpottaen kehittämistä ja integraatiota verkkopalveluihin.
