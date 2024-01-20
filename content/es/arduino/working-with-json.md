---
title:                "Trabajando con JSON"
html_title:           "Bash: Trabajando con JSON"
simple_title:         "Trabajando con JSON"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/working-with-json.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

JSON, que significa JavaScript Object Notation, es un formato ligero de intercambio de datos. Los programadores lo usan porque es fácil de leer para humanos y sencillo de analizar para máquinas, facilitando la comunicación entre sistemas.

## Cómo Hacerlo:

Primero, asegúrate de tener la librería `ArduinoJson`, la cual puedes instalar desde el Gestor de Librerías en el IDE de Arduino.

```c++
#include <ArduinoJson.h>

void setup() {
  Serial.begin(9600);
  // Creando un objeto JSON
  StaticJsonDocument<200> doc;

  doc["temperatura"] = 23.5;
  doc["humedad"] = 80;

  // Serializando JSON para enviar o guardar
  serializeJson(doc, Serial);
}

void loop() {
  // Este código es solo para ilustrar la configuración
}
```
Salida de ejemplo:
```
{"temperatura":23.5,"humedad":80}
```

Para leer JSON:
```c++
#include <ArduinoJson.h>

const char* json = "{\"temperatura\":23.5,\"humedad\":80}";

void setup() {
  Serial.begin(9600);
  
  StaticJsonDocument<200> doc;
  deserializeJson(doc, json);

  float temperatura = doc["temperatura"];
  int humedad = doc["humedad"];

  Serial.println(temperatura);
  Serial.println(humedad);
}

void loop() {
  // Este código es solo para ilustrar la configuración
}
```
Salida de ejemplo:
```
23.50
80
```

## Profundización:

JSON se originó a partir de la notación de objetos en JavaScript, introducido en los primeros años del 2000. Alternativas incluyen XML y YAML, aunque JSON prevalece por su simplicidad y eficiencia en el procesamiento. Para implementarlo en Arduino, utilizamos la biblioteca `ArduinoJson`, que es eficiente y fácil de usar, incluso en dispositivos con memoria limitada.

## Ver También:

- [Página oficial de ArduinoJson](https://arduinojson.org/)
- [Referencia JSON de Mozilla Developer Network](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/JSON)
- [Tutorial JSON en w3schools](https://www.w3schools.com/js/js_json_intro.asp)