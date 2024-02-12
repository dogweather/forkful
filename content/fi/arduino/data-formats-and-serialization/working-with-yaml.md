---
title:                "Työskentely YAML:n kanssa"
aliases: - /fi/arduino/working-with-yaml.md
date:                  2024-02-03T19:24:49.620194-07:00
model:                 gpt-4-0125-preview
simple_title:         "Työskentely YAML:n kanssa"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?

YAML (YAML Ain't Markup Language) on ihmisen luettavissa oleva datan serialisointistandardi, jota voidaan käyttää määritystiedostoissa, ohjelmien välisessä viestinnässä ja datan tallennuksessa. Ohjelmoijat kääntyvät YAMLin puoleen Arduino-projekteissa, jotta sovellusten määritysprosessia voidaan virtaviivaistaa, mikä helpottaa parametrien muokkaamista syvälle koodiin sukeltamatta, parantaa luettavuutta ja yksinkertaistaa määritysten jakamista.

## Kuinka:

YAMLin käyttäminen suoraan Arduinossa ei ole yhtä suoraviivaista kuin korkeamman tason ohjelmointiympäristöissä muistirajoitusten ja natiivien YAML-käsittelykirjastojen puuttumisen vuoksi. Kuitenkin projekteille, jotka vaativat YAMLin jäsentämistä tai generointia, tyypillinen lähestymistapa sisältää avustavan tietokoneen käytön (kuten Raspberry Pi) tai YAML-tiedostojen muuntamisen Arduino-ystävällisempään muotoon (kuten JSON) käyttäen ulkoisia skriptejä. Demonstrointitarkoituksessa keskitymme jälkimmäiseen lähestymistapaan käyttäen suosittua kirjastoa: ArduinoJson.

**Vaihe 1:** Muunna YAML-määrityksesi JSON-muotoon. Voit käyttää online-työkaluja tai komentorivin apuohjelmia, kuten `yq`.

YAML-tiedosto (`config.yaml`):
```yaml
wifi:
  ssid: "YourSSID"
  salasana: "YourPassword"
```

Muunnettu JSON-muotoon (`config.json`):
```json
{
  "wifi": {
    "ssid": "YourSSID",
    "salasana": "YourPassword"
  }
}
```

**Vaihe 2:** Käytä ArduinoJson-kirjastoa jäsentämään JSON-tiedosto Arduino-luonnoksessasi. Ensin sinun täytyy asentaa ArduinoJson-kirjasto kirjastohallinnan kautta Arduino IDE:ssa.

**Vaihe 3:** Lataa ja jäsenä JSON koodissasi. Arduino-laitteen tallennustilan rajoitusten vuoksi kuvittele, että JSON-merkkijono on tallennettu muuttujaan tai luettu SD-kortilta.

Esimerkki Arduino-luonnoksesta:
```cpp
#include <ArduinoJson.h>

const char* jsonConfig = "{\"wifi\":{\"ssid\":\"YourSSID\",\"salasana\":\"YourPassword\"}}";

void setup() {
  Serial.begin(9600);

  StaticJsonDocument<200> doc;
  DeserializationError error = deserializeJson(doc, jsonConfig);

  if (error) {
    Serial.print(F("deserializeJson() epäonnistui: "));
    Serial.println(error.f_str());
    return;
  }

  const char* ssid = doc["wifi"]["ssid"]; // "YourSSID"
  const char* salasana = doc["wifi"]["salasana"]; // "YourPassword"

  Serial.print("SSID: ");
  Serial.println(ssid);
  Serial.print("Salasana: ");
  Serial.println(salasana);
}

void loop() {
  // Ei mitään tässä esimerkissä
}
```

Tuloste luonnoksen suorittamisen jälkeen:
```
SSID: YourSSID
Salasana: YourPassword
```

Tämä lähestymistapa, joka sisältää muuntamisen JSON-muotoon ja ArduinoJson-kirjaston hyödyntämisen, mahdollistaa hallittavan YAML-määritysten käsittelyn Arduino-projekteissa, ohittaen suoran YAML-jäsentämisen mikrokontrollerissa.
