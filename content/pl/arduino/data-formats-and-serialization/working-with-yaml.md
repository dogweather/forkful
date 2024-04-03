---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:09.721826-07:00
description: "YAML (YAML Ain't Markup Language) to standard serializacji danych czytelnych\
  \ dla cz\u0142owieka, kt\xF3ry mo\u017Ce by\u0107 u\u017Cywany dla plik\xF3w konfiguracyjnych,\u2026"
lastmod: '2024-03-13T22:44:35.691754-06:00'
model: gpt-4-0125-preview
summary: "YAML (YAML Ain't Markup Language) to standard serializacji danych czytelnych\
  \ dla cz\u0142owieka, kt\xF3ry mo\u017Ce by\u0107 u\u017Cywany dla plik\xF3w konfiguracyjnych,\
  \ komunikacji mi\u0119dzyprogramowej i przechowywania danych."
title: Praca z YAML
weight: 41
---

## Jak to zrobić:
Praca z YAML bezpośrednio na Arduino nie jest tak prosta jak w środowiskach programistycznych wyższego poziomu ze względu na ograniczenia pamięciowe i brak natywnych bibliotek przetwarzających YAML. Jednak w projektach wymagających parsowania lub generowania YAML, typowe podejście obejmuje użycie komputera towarzyszącego (takiego jak Raspberry Pi) lub konwersję plików YAML do formatu bardziej przyjaznego dla Arduino (takiego jak JSON) za pomocą zewnętrznych skryptów. W celach demonstracyjnych skupimy się na tym drugim podejściu, korzystając z popularnej biblioteki: ArduinoJson.

**Krok 1:** Przekonwertuj swoją konfigurację YAML na JSON. Możesz użyć narzędzi online lub narzędzi linii poleceń takich jak `yq`.

Plik YAML (`config.yaml`):
```yaml
wifi:
  ssid: "YourSSID"
  password: "YourPassword"
```

Przekonwertowany na JSON (`config.json`):
```json
{
  "wifi": {
    "ssid": "YourSSID",
    "password": "YourPassword"
  }
}
```

**Krok 2:** Użyj biblioteki ArduinoJson do parsowania pliku JSON w twoim szkicu Arduino. Najpierw musisz zainstalować bibliotekę ArduinoJson za pomocą Menedżera bibliotek w środowisku Arduino IDE.

**Krok 3:** Wczytaj i przetłumacz JSON w swoim kodzie. Ze względu na ograniczenia pamięciowe Arduino, wyobraź sobie, że ciąg JSON jest przechowywany w zmiennej lub odczytywany z karty SD.

Przykładowy szkic Arduino:
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
  // Tutaj nic w tym przykładzie
}
```

Wynik po uruchomieniu szkicu:
```
SSID: YourSSID
Password: YourPassword
```

To podejście, polegające na konwersji do JSON i wykorzystaniu biblioteki ArduinoJson, umożliwia łatwe zarządzanie konfiguracją YAML w projektach Arduino, omijając bezpośrednie parsowanie YAML na mikrokontrolerze.
