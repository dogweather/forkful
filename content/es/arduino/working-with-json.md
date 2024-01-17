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

## ¿Qué es JSON y por qué lo usamos?

JSON es un formato de intercambio de datos que permite a los programadores almacenar y transmitir información estructurada. Lo utilizamos para representar datos en un formato fácilmente legible por humanos y también por máquinas. Con JSON, podemos organizar y compartir datos de manera eficiente entre diferentes sistemas y aplicaciones.

## Cómo hacerlo:

Usar JSON en Arduino es muy sencillo gracias a la librería "ArduinoJson". Primero, debemos descargar e instalar esta librería en nuestro IDE de Arduino. Luego, podemos seguir estos pasos:

1. Incluir la librería en nuestro código con `#include <ArduinoJson.h>`.

2. Crear un objeto "json" usando el constructor de la librería: `DynamicJsonDocument json(512);` (El 512 es el tamaño máximo en bytes para nuestro JSON. Puede cambiar según sus necesidades).

3. Agregar datos al objeto usando el método `json["nombre"] = valor;`. Por ejemplo: `json["nombre"] = "Juan";`.

4. Convertir el objeto JSON en un string utilizando el método `serializeJson(json, buffer, size);` donde "buffer" es una variable de tipo "char" donde se almacenará el JSON y "size" es el tamaño máximo en bytes del buffer.

5. ¡Listo! Ahora podemos usar el string "buffer" para transmitir nuestros datos en formato JSON.

## Inmersión profunda:

JSON fue creado en 2001 por Douglas Crockford como una alternativa al formato de intercambio de datos XML. Aunque XML sigue siendo ampliamente utilizado, JSON es más ligero y rápido debido a su estructura basada en pares de clave-valor. Además, la librería "ArduinoJson" implementa una técnica llamada "parsing incremental" que reduce significativamente el tiempo y memoria necesarios para trabajar con JSON en dispositivos como Arduino.

## Vea también:

- [Documentación oficial de ArduinoJson](https://arduinojson.org/)
- [Tutorial de ArduinoJson de Adafruit](https://learn.adafruit.com/arduino-json)
- [Introducción a JSON de W3Schools](https://www.w3schools.com/js/js_json_intro.asp)