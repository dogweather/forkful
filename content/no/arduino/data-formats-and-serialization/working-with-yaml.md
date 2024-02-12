---
title:                "Arbeider med YAML"
aliases: - /no/arduino/working-with-yaml.md
date:                  2024-02-03T19:25:28.805058-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeider med YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?

YAML (YAML Ain't Markup Language) er en menneskelesbar standard for serialisering av data som kan brukes for konfigurasjonsfiler, kommunikasjon mellom programmer og datalagring. Programmerere vender seg til YAML for Arduino-prosjekter for å strømlinjeforme konfigurasjonsprosessen til applikasjonene sine, for å gjøre det enklere å endre parametere uten å dykke dypt inn i koden, forbedre lesbarheten, og gjøre deling av konfigurasjon enklere.

## Hvordan:

Å jobbe direkte med YAML på Arduino er ikke så rett frem som i programmeringsmiljøer på høyere nivå på grunn av begrensninger i minnet og fraværet av innebygde YAML-behandlingsbiblioteker. Imidlertid, for prosjekter som krever YAML-tolkning eller generering, innebærer en typisk tilnærming å bruke en ledsagerdatamaskin (som en Raspberry Pi) eller å konvertere YAML-filer til et mer Arduino-vennlig format (som JSON) ved hjelp av eksterne skript. For demonstrasjonsformål, la oss fokusere på den siste tilnærmingen ved hjelp av et populært bibliotek: ArduinoJson.

**Steg 1:** Konverter din YAML-konfigurasjon til JSON. Du kan bruke nettbaserte verktøy eller kommandolinje-verktøy som `yq`.

YAML-fil (`config.yaml`):
```yaml
wifi:
  ssid: "YourSSID"
  password: "YourPassword"
```

Konvertert til JSON (`config.json`):
```json
{
  "wifi": {
    "ssid": "YourSSID",
    "password": "YourPassword"
  }
}
```

**Steg 2:** Bruk ArduinoJson-biblioteket til å tolke JSON-filen i din Arduino-sketch. Først må du installere ArduinoJson-biblioteket via biblioteksbehandleren i Arduino IDE.

**Steg 3:** Last og tolk JSON i koden din. På grunn av Arduino's lagringsbegrensninger, forestill deg at JSON-strengen er lagret i en variabel eller lest fra et SD-kort.

Eksempel Arduino-sketch:
```cpp
#include <ArduinoJson.h>

const char* jsonConfig = "{\"wifi\":{\"ssid\":\"YourSSID\",\"password\":\"YourPassword\"}}";

void setup() {
  Serial.begin(9600);

  StaticJsonDocument<200> doc;
  DeserializationError error = deserializeJson(doc, jsonConfig);

  if (error) {
    Serial.print(F("deserializeJson() feilet: "));
    Serial.println(error.f_str());
    return;
  }

  const char* ssid = doc["wifi"]["ssid"]; // "YourSSID"
  const char* password = doc["wifi"]["password"]; // "YourPassword"

  Serial.print("SSID: ");
  Serial.println(ssid);
  Serial.print("Passord: ");
  Serial.println(password);
}

void loop() {
  // Ingenting her for dette eksempelet
}
```

Output ved kjøring av sketsjen:
```
SSID: YourSSID
Passord: YourPassword
```

Denne tilnærmingen, som involverer konvertering til JSON og utnyttelse av ArduinoJson-biblioteket, gir håndterlig YAML-konfigurasjonsbehandling innenfor Arduino-prosjekter, og omgår direkte YAML-tolkning på mikrokontrolleren.
