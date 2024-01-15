---
title:                "Trabajando con json"
html_title:           "Arduino: Trabajando con json"
simple_title:         "Trabajando con json"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/working-with-json.md"
---

{{< edit_this_page >}}

## ¿Por qué trabajar con JSON en Arduino?

JSON (JavaScript Object Notation) es un formato de intercambio de datos ligero y fácil de entender, que ha ganado popularidad en los últimos años. Al utilizar JSON en los proyectos de Arduino, puedes integrar tu dispositivo con otros sistemas de forma sencilla y efectiva.

## Cómo hacerlo

Para empezar a trabajar con JSON en Arduino, necesitas tener instalada la librería `ArduinoJson`. Puedes hacerlo descargándola desde el gestor de librerías o a través de la página de Arduino. Una vez instalada, puedes seguir estos pasos para utilizar JSON en tus proyectos:

1. Include la librería `ArduinoJson` en tu sketch.

```Arduino
#include <ArduinoJson.h>
```

2. Define un objeto `DynamicJsonDocument` y especifica el tamaño máximo del documento.

```Arduino
DynamicJsonDocument jsonDocument(1024);
```

3. Ahora, ya puedes añadir los datos que deseas enviar en formato JSON al objeto. Por ejemplo, vamos a crear un objeto con un array que contenga algunas letras del abecedario.

```Arduino
char letters[] = {'a', 'b', 'c', 'd', 'e'};
jsonDocument["abecedario"] = letters;
```

4. Una vez que tengas todos los datos que deseas enviar, puedes imprimir el objeto en formato JSON con `serializeJson()`.

```Arduino
serializeJson(jsonDocument, Serial);
```

5. Si deseas recibir datos en formato JSON, puedes hacerlo utilizando `DeserializationFeature`. Vamos a utilizar un ejemplo en el que recibimos un objeto con información meteorológica, que incluye la temperatura y la humedad.

```Arduino
StaticJsonDocument<200> jsonDocument;
DeserializationError error = deserializeJson(jsonDocument, Serial);
if (error) {
  Serial.println("Error al recibir datos en formato JSON");
} else {
  float temperatura = jsonDocument["temperatura"];
  float humedad = jsonDocument["humedad"];
  Serial.print("Temperatura: ");
  Serial.println(temperatura);
  Serial.print("Humedad: ");
  Serial.println(humedad);
}
```

¡Y eso es todo! Con estos pasos, ya puedes enviar y recibir datos en formato JSON en tus proyectos de Arduino.

## Profundizando en JSON

Si deseas aprender más sobre JSON y cómo utilizarlo en tus proyectos de Arduino, puedes consultar la documentación de la librería `ArduinoJson` o explorar más ejemplos en la página oficial de Arduino.

## Ver también

- [Página oficial de Arduino](https://www.arduino.cc)
- [Página oficial de ArduinoJson](https://arduinojson.org)
- [Documentación de ArduinoJson](https://arduinojson.org/v6/api)