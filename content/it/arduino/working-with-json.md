---
title:                "Lavorare con JSON"
date:                  2024-01-19
simple_title:         "Lavorare con JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/working-with-json.md"
---

{{< edit_this_page >}}

## Che cosa e perché?

JSON, acronimo di JavaScript Object Notation, è un formato leggero per lo scambio di dati. Programmatori lo usano perché è semplice da leggere e scrivere per gli umani, e facile da analizzare e generare per le macchine.

## Come fare:

Per lavorare con JSON in Arduino, avrai bisogno della libreria `ArduinoJson`. Installala tramite il Library Manager in Arduino IDE. Ecco un esempio semplice:

```c++
#include <ArduinoJson.h>

void setup() {
  Serial.begin(9600);

  // Crea un oggetto JSON
  StaticJsonDocument<200> doc;
  doc["temperatura"] = 23.5;
  doc["umidità"] = 48.2;

  // Serializza JSON e invialo alla seriale
  serializeJson(doc, Serial);
}

void loop() {
  // Funzioni non necessarie per questo esempio
}
```
Output della seriale:
```
{"temperatura":23.5,"umidità":48.2}
```

## Approfondimento:

La libreria `ArduinoJson` è stata creata da Benoit Blanchon e rilasciata intorno al 2013. Rispetto ad altre soluzioni come la manipolazione manuale delle stringhe, `ArduinoJson` offre un modo robusto e ottimizzato per il parsing e la serializzazione di JSON, essenziale per IoT e progetti web con Arduino. La libreria utilizza la deserializzazione "zero-copy" per elevata efficienza in ambienti con risorse limitate.

## Vedi anche:

- Documentazione ufficiale ArduinoJson: [https://arduinojson.org/](https://arduinojson.org/)
- GitHub di ArduinoJson: [https://github.com/bblanchon/ArduinoJson](https://github.com/bblanchon/ArduinoJson)
- Introduzione a JSON: [https://www.json.org/json-it.html](https://www.json.org/json-it.html)
