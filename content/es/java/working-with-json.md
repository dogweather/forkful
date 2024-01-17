---
title:                "Trabajando con json"
html_title:           "Java: Trabajando con json"
simple_title:         "Trabajando con json"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/working-with-json.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Trabajar con JSON es una forma común para que los programadores intercambien datos entre aplicaciones. JSON es un formato ligero y fácil de leer que se utiliza para almacenar y transmitir datos estructurados. Los programadores utilizan JSON porque les permite transferir datos de manera eficiente y consistente entre diferentes sistemas.

## Cómo hacerlo:

```Java
// Crear un objeto JSON
JSONObject persona = new JSONObject();

// Agregar propiedades al objeto
persona.put("nombre", "Juan");
persona.put("edad", 25);
persona.put("ciudad", "Madrid");
persona.put("aficiones", new JSONArray().put("correr").put("leer").put("pintar"));

// Convertir a formato String
String personaJSON = persona.toString();

// Imprimir el resultado
System.out.println(personaJSON);

// Resultado: {"nombre":"Juan","edad":25,"ciudad":"Madrid","aficiones":["correr","leer","pintar"]}
```

## Profundizando

JSON, o JavaScript Object Notation, fue creado en 2001 como una alternativa más ligera a XML para intercambiar datos entre aplicaciones. Sin embargo, a lo largo de los años, JSON se ha vuelto cada vez más popular debido a su facilidad de lectura y su capacidad para ser interpretado por diferentes lenguajes de programación. Además, existen muchas librerías y herramientas que hacen que trabajar con JSON sea aún más sencillo.

Otras alternativas a JSON son XML, CSV y YAML. Sin embargo, JSON sigue siendo una de las opciones más comunes para el intercambio de datos debido a su simplicidad y compatibilidad con la mayoría de los lenguajes de programación.

Para trabajar con JSON en Java, es necesario utilizar una librería como Google Gson o Jackson. Estas librerías permiten convertir objetos Java en formato JSON y viceversa, facilitando el manejo de datos estructurados en aplicaciones.

## Ver también

- [Documentación oficial de JSON](https://www.json.org/json-en.html)
- [Tutorial de Google Gson](https://github.com/google/gson/blob/master/UserGuide.md)
- [Documentación de Jackson](https://github.com/FasterXML/jackson-docs)