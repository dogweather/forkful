---
title:                "Trabajando con yaml"
html_title:           "Arduino: Trabajando con yaml"
simple_title:         "Trabajando con yaml"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/working-with-yaml.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

Trabajar con YAML es una forma de organizar y manejar datos en tus programas de Arduino. Los programadores usan YAML porque es una forma fácil y eficiente de guardar y acceder a información en sus proyectos.

## Cómo hacerlo:

Para trabajar con YAML en Arduino, necesitarás la biblioteca "Arduino-esp8266fs-plugin". Una vez que la tengas instalada, puedes utilizar el siguiente código como ejemplo:

```
Arduino-esp8266fs-plugin.verbose=true
Arduino-esp8266fs-plugin.path=[directorio-raiz]
```

El primer comando permite que se muestren los detalles del proceso en la consola. El segundo comando especifica la carpeta raíz donde se guardarán los datos YAML. Luego, puedes utilizar las funciones `SPIFFS.begin()` y `SPIFFS.exists()` para leer y escribir datos YAML en tus archivos.

## Profundizando:

El formato YAML (YAML Ain't Markup Language) fue creado en 2001 como una alternativa al formato XML para guardar datos estructurados. Además de Arduino, YAML también es ampliamente utilizado en otros lenguajes de programación como Python y Ruby.

Si no quieres utilizar la biblioteca "Arduino-esp8266fs-plugin", otra opción es usar la biblioteca "ArduinoJson" para trabajar con datos YAML en Arduino. Sin embargo, esta biblioteca tiene una curva de aprendizaje más larga y requiere un poco más de código para implementar.

Cuando trabajas con YAML en Arduino, asegúrate de utilizar correctamente la sintaxis y los espacios en blanco, ya que cualquier error puede provocar un comportamiento inesperado en tu programa.

## Ver también:

- [Documentación de la biblioteca "Arduino-esp8266fs-plugin"](https://github.com/esp8266/arduino-esp8266fs-plugin#how-to-install)
- [Documentación de la biblioteca "ArduinoJson"](https://arduinojson.org/)