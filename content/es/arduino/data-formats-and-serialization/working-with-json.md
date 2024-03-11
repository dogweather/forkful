---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:33.473104-07:00
description: "JSON, que significa Notaci\xF3n de Objetos JavaScript, es un formato\
  \ ligero de intercambio de datos, lo que lo hace perfecto para almacenamiento de\
  \ datos o\u2026"
lastmod: '2024-03-11T00:14:33.177542-06:00'
model: gpt-4-0125-preview
summary: "JSON, que significa Notaci\xF3n de Objetos JavaScript, es un formato ligero\
  \ de intercambio de datos, lo que lo hace perfecto para almacenamiento de datos\
  \ o\u2026"
title: Trabajando con JSON
---

{{< edit_this_page >}}

## Qué y Por Qué?

JSON, que significa Notación de Objetos JavaScript, es un formato ligero de intercambio de datos, lo que lo hace perfecto para almacenamiento de datos o archivos de configuración en proyectos Arduino. Los programadores lo utilizan por su simplicidad y legibilidad en diversos entornos de programación, incluido Arduino, permitiendo un intercambio de datos sin problemas con APIs web u otros sistemas.

## Cómo:

Para trabajar con JSON en Arduino, la biblioteca `ArduinoJson` es una opción popular debido a su facilidad de uso y eficiencia. Permite parsear cadenas JSON, modificarlas y serializar objetos de nuevo en cadenas JSON. Aquí te mostramos cómo usarla:

1. **Instalar la biblioteca ArduinoJson**: Utiliza el Gestor de Bibliotecas en el IDE de Arduino e instala "ArduinoJson".

2. **Deserializar una cadena JSON**: Aquí te mostramos cómo parsear una cadena JSON y extraer valores.

```cpp
#include <ArduinoJson.h>

const char* json = "{\"sensor\":\"gps\",\"time\":1351824120,\"data\":[48.756080,2.302038]}";

void setup() {
  Serial.begin(9600);
  StaticJsonDocument<200> doc; // Ajusta el tamaño de acuerdo con el documento JSON
  DeserializationError error = deserializeJson(doc, json);

  if (error) {
    Serial.print(F("deserializeJson() falló: "));
    Serial.println(error.f_str());
    return;
  }

  const char* sensor = doc["sensor"]; // "gps"
  long time = doc["time"]; // 1351824120
  float latitud = doc["data"][0]; // 48.756080
  float longitud = doc["data"][1]; // 2.302038
  
  Serial.println(sensor);
  Serial.println(time);
  Serial.println(latitud, 6);
  Serial.println(longitud, 6);
}

void loop() {
  // Bucle vacío
}
```

Salida de ejemplo:

```
gps
1351824120
48.756080
2.302038
```

3. **Serializar a una cadena JSON**: Aquí te mostramos cómo crear una cadena JSON a partir de datos.

```cpp
#include <ArduinoJson.h>

void setup() {
  Serial.begin(9600);

  StaticJsonDocument<200> doc; // Ajusta el tamaño de acuerdo con los datos
  doc["sensor"] = "gps";
  doc["time"] = 1351824120;
  JsonArray datos = doc.createNestedArray("data");
  datos.add(48.756080);
  datos.add(2.302038);

  serializeJson(doc, Serial);
}

void loop() {
  // Bucle vacío
}
```

Salida de ejemplo (formateada para legibilidad):

```
{"sensor":"gps","time":1351824120,"data":[48.756080,2.302038]}
```

Usar efectivamente la biblioteca `ArduinoJson` permite que los proyectos Arduino comuniquen estructuras de datos complejas en un formato legible por humanos, facilitando el desarrollo e integración con servicios web.
