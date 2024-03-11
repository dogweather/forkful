---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:40.950037-07:00
description: "YAML (YAML Ain't Markup Language) \xE8 uno standard di serializzazione\
  \ di dati leggibile dall'uomo che pu\xF2 essere utilizzato per file di configurazione,\u2026"
lastmod: '2024-03-11T00:14:17.322144-06:00'
model: gpt-4-0125-preview
summary: "YAML (YAML Ain't Markup Language) \xE8 uno standard di serializzazione di\
  \ dati leggibile dall'uomo che pu\xF2 essere utilizzato per file di configurazione,\u2026"
title: Lavorare con YAML
---

{{< edit_this_page >}}

## Cos'è & Perché?

YAML (YAML Ain't Markup Language) è uno standard di serializzazione di dati leggibile dall'uomo che può essere utilizzato per file di configurazione, comunicazione tra programmi e archiviazione di dati. I programmatori si rivolgono a YAML per i progetti Arduino per semplificare il processo di configurazione delle loro applicazioni, rendendo più facile modificare i parametri senza immergersi nel codice, migliorando la leggibilità e semplificando la condivisione delle configurazioni.

## Come fare:

Lavorare direttamente con YAML su Arduino non è semplice come negli ambienti di programmazione di livello superiore a causa dei vincoli di memoria e dell'assenza di librerie native per l'elaborazione di YAML. Tuttavia, per progetti che richiedono l'analisi o la generazione di YAML, un approccio tipico comporta l'utilizzo di un computer di supporto (come un Raspberry Pi) o la conversione dei file YAML in un formato più adatto ad Arduino (come JSON) utilizzando script esterni. A scopo dimostrativo, concentriamoci su quest'ultimo approccio utilizzando una libreria popolare: ArduinoJson.

**Passo 1:** Converti la tua configurazione YAML in JSON. Puoi utilizzare strumenti online o utility da riga di comando come `yq`.

File YAML (`config.yaml`):
```yaml
wifi:
  ssid: "YourSSID"
  password: "YourPassword"
```

Convertito in JSON (`config.json`):
```json
{
  "wifi": {
    "ssid": "YourSSID",
    "password": "YourPassword"
  }
}
```

**Passo 2:** Usa la libreria ArduinoJson per analizzare il file JSON nel tuo sketch Arduino. Prima di tutto, devi installare la libreria ArduinoJson tramite il Library Manager nell'IDE di Arduino.

**Passo 3:** Carica e analizza il JSON nel tuo codice. A causa dei limiti di memoria di Arduino, immagina che la stringa JSON sia memorizzata in una variabile o letta da una scheda SD.

Esempio di sketch Arduino:
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
  // Niente qui per questo esempio
}
```

Output alla esecuzione dello sketch:
```
SSID: YourSSID
Password: YourPassword
```

Questo approccio, che implica la conversione in JSON e sfrutta la libreria ArduinoJson, consente una gestione gestibile della configurazione YAML all'interno dei progetti Arduino, eludendo l'analisi diretta di YAML sul microcontrollore.
