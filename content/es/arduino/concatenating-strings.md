---
title:                "Uniendo cadenas"
html_title:           "Arduino: Uniendo cadenas"
simple_title:         "Uniendo cadenas"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

La concatenación de cadenas es un concepto importante en la programación de Arduino, que básicamente significa unir dos o más cadenas de texto en una sola. Los programadores lo hacen para crear textos más largos y complejos, así como para formatear y mostrar información en la pantalla.

## Cómo hacerlo:

Para concatenar cadenas en Arduino, podemos usar la función `+` (más). Por ejemplo, si tenemos dos cadenas `nombre` y `apellido` y queremos unirlas en una sola, podemos hacerlo así:

```Arduino
String nombre = "Juan";
String apellido = "Pérez";
String nombreCompleto = nombre + " " + apellido;
```

El resultado será una nueva cadena llamada `nombreCompleto` que contiene "Juan Pérez". También podemos concatenar cadenas con números o variables:

```Arduino
int edad = 26;
String frase = "Tengo " + String(edad) + " años.";
```

En este caso, convertimos la variable `edad` en una cadena usando `String()` dentro de la función `+`.

## Profundizando:

La concatenación de cadenas ha sido una técnica ampliamente utilizada en la programación desde los primeros lenguajes de programación. En Arduino, también podemos usar la función `concat()` para concatenar cadenas, pero es menos eficiente que usar la función `+`.

Una alternativa a la concatenación de cadenas es usar la función `sprintf()`, que permite formatear cadenas con variables y valores.

En términos de implementación, la concatenación de cadenas en Arduino se realiza usando la clase `String`. Esto permite una manipulación más sencilla de las cadenas, sin necesidad de preocuparse por la gestión de memoria.

## Ver también:

- Documentación oficial de la función `+` en Arduino:
https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/concat/
- Tutorial sobre la función `sprintf()` en Arduino:
https://forum.arduino.cc/index.php?topic=148027.0
- Guía completa sobre el uso de cadenas en Arduino:
https://www.arduino.cc/reference/es/language/variables/data-types/string/