---
title:                "Arduino: Trabajando con json"
simple_title:         "Trabajando con json"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/working-with-json.md"
---

{{< edit_this_page >}}

¿Por qué trabajar con JSON en Arduino?

Arduino es una plataforma de hardware y software de código abierto que permite a los usuarios crear proyectos electrónicos de todo tipo. La capacidad de trabajar con JSON en Arduino permite a los desarrolladores interactuar con datos estructurados de forma sencilla, lo que abre una gran cantidad de posibilidades para proyectos más avanzados.

## ¿Cómo hacerlo?

Para trabajar con JSON en Arduino, lo primero que debemos hacer es importar la librería correspondiente. En la mayoría de los casos, la librería más utilizada es "ArduinoJson" que se puede descargar directamente desde el gestor de librerías de Arduino IDE. Una vez importada la librería, podemos comenzar a interactuar con datos en formato JSON.

Un ejemplo simple de cómo leer datos JSON es el siguiente:

```Arduino
#include <ArduinoJson.h> // Importar la librería
String datosJSON = "{\"nombre\":\"Juan\", \"edad\":25}"; // Definir el objeto JSON como una cadena de caracteres
StaticJsonDocument<200> doc; // Crear un documento de tipo JSON para almacenar los datos
DeserializationError error = deserializeJson(doc, datosJSON); // Deserializar los datos JSON en el documento
if (!error) { // Verificar si no hubo errores
    String nombre = doc["nombre"]; // Obtener el valor correspondiente a la propiedad "nombre"
    int edad = doc["edad"]; // Obtener el valor correspondiente a la propiedad "edad"
    Serial.println("Nombre: " + nombre); // Imprimir el nombre en el monitor serial
    Serial.println("Edad: " + String(edad)); // Imprimir la edad en el monitor serial
}
```

Este es un ejemplo básico, pero se puede adaptar según las necesidades de cada proyecto. También se pueden realizar operaciones como escribir datos en formato JSON o enviarlos a través de una conexión de red como WiFi o Ethernet.

## Profundizando

Trabajar con JSON en Arduino puede ser de gran utilidad para proyectos que involucren la lectura y escritura de datos estructurados. Además de la librería mencionada anteriormente, existen otras opciones que ofrecen diferentes funcionalidades y compatibilidades con distintas versiones de Arduino. También es importante recordar que los datos en formato JSON deben seguir una estructura específica para poder ser leídos correctamente.

Para profundizar aún más en el tema, es recomendable revisar la documentación oficial de Arduino y explorar diferentes ejemplos y tutoriales en línea. Al dominar el trabajo con JSON, se pueden crear proyectos más complejos y dinámicos que abarquen más áreas de interés.

## Ver también

- [Documentación de Arduino sobre JSON](https://www.arduino.cc/en/Reference/ArduinoJson)
- [Ejemplos de proyectos con JSON en Arduino](https://create.arduino.cc/projecthub/projects/tags/json)
- [Otras librerías para trabajar con JSON en Arduino](https://www.arduino.cc/reference/en/libraries/category/data-processing/json/)