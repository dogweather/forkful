---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:01.159986-07:00
description: "Hur man g\xF6r: Att arbeta med YAML direkt p\xE5 Arduino \xE4r inte\
  \ lika rakt p\xE5 sak som i milj\xF6er med h\xF6gre programmeringsniv\xE5 p\xE5\
  \ grund av begr\xE4nsningar i minne\u2026"
lastmod: '2024-03-13T22:44:38.188267-06:00'
model: gpt-4-0125-preview
summary: "Att arbeta med YAML direkt p\xE5 Arduino \xE4r inte lika rakt p\xE5 sak\
  \ som i milj\xF6er med h\xF6gre programmeringsniv\xE5 p\xE5 grund av begr\xE4nsningar\
  \ i minne och fr\xE5nvaro av inbyggda YAML-behandlingsbibliotek."
title: Att Arbeta med YAML
weight: 41
---

## Hur man gör:
Att arbeta med YAML direkt på Arduino är inte lika rakt på sak som i miljöer med högre programmeringsnivå på grund av begränsningar i minne och frånvaro av inbyggda YAML-behandlingsbibliotek. Men, för projekt som kräver YAML-tolkning eller generering är en typisk metod att använda en kompanjonsdator (som en Raspberry Pi) eller konvertera YAML-filer till ett format som är mer vänligt för Arduino (som JSON) med hjälp av externa skript. För demonstrationsändamål, låt oss fokusera på det senare tillvägagångssättet genom att använda ett populärt bibliotek: ArduinoJson.

**Steg 1:** Konvertera din YAML-konfiguration till JSON. Du kan använda verktyg online eller kommandoradsverktyg som `yq`.

YAML-fil (`config.yaml`):
```yaml
wifi:
  ssid: "YourSSID"
  password: "YourPassword"
```

Omvandlad till JSON (`config.json`):
```json
{
  "wifi": {
    "ssid": "YourSSID",
    "password": "YourPassword"
  }
}
```

**Steg 2:** Använd ArduinoJson-biblioteket för att tolka JSON-filen i din Arduino-sketch. Först måste du installera ArduinoJson-biblioteket via bibliotekshanteraren i Arduino IDE.

**Steg 3:** Ladda och tolka JSON i din kod. På grund av Arduinos lagringsbegränsningar, föreställ dig att JSON-strängen är lagrad i en variabel eller läses från ett SD-kort.

Exempel på Arduino-sketch:
```cpp
#include <ArduinoJson.h>

const char* jsonConfig = "{\"wifi\":{\"ssid\":\"YourSSID\",\"password\":\"YourPassword\"}}";

void setup() {
  Serial.begin(9600);

  StaticJsonDocument<200> doc;
  DeserializationError error = deserializeJson(doc, jsonConfig);

  if (error) {
    Serial.print(F("deserializeJson() failed: "));
    Serial.println(error.f_str());
    return;
  }

  const char* ssid = doc["wifi"]["ssid"]; // "YourSSID"
  const char* password = doc["wifi"]["password"]; // "YourPassword"

  Serial.print("SSID: ");
  Serial.println(ssid);
  Serial.print("Password: ");
  Serial.println(password);
}

void loop() {
  // Inget här för detta exempel
}
```

Utskrift vid körning av sketchen:
```
SSID: YourSSID
Password: YourPassword
```

Detta tillvägagångssätt, som innebär konvertering till JSON och användning av ArduinoJson-biblioteket, möjliggör hantering av YAML-konfiguration inom Arduino-projekt på ett hanterbart sätt, genom att kringgå direkt YAML-tolkning på mikrokontrollern.
