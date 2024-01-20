---
title:                "Interpolando una cadena de texto"
html_title:           "Haskell: Interpolando una cadena de texto"
simple_title:         "Interpolando una cadena de texto"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/interpolating-a-string.md"
---

{{< edit_this_page >}}

# Interpolación de Strings en Java

## ¿Qué y Por Qué?

La interpolación de strings es una técnica que permite insertar valores de variables en los strings. Los programadores lo hacen para construir strings dinámicamente y mejorar la legibilidad del código.

## Cómo se hace:
En Java actualmente usamos la clase `String.format()`. Aquí tienes un ejemplo sencillo.
```Java
String nombre = "Juan";
int edad = 25;
String saludo = String.format("Hola %s, tienes %d años.", nombre, edad);
System.out.println(saludo); 
```
Este código imprime: `Hola Juan, tienes 25 años.`

## Inmersión profunda

La interpolación de strings ha sido popular en otros lenguajes como Ruby y JavaScript mucho antes que en Java. Pero en las versiones más recientes de Java se incorporó la función `String.format()` para facilitar esta tarea.

En alternativa, puedes usar la concatenación de strings con el operador `+`, pero la interpolación proporciona un código más limpio y menos propenso a errores.

En cuanto a los detalles de implementación, cuando usas `String.format()`, internamente, Java utiliza la clase `Formatter` para procesar el string y reemplazar los lugares donde deberían ir los valores de las variables.

## Consulta también 
Para los que quieren profundizar en el tema, os dejo unos enlaces de interés:

- [El manejo de strings en Java](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/String.html)
- [Interpolación de strings en otros lenguajes](https://en.wikipedia.org/wiki/String_interpolation)
- [Otras técnicas de formateo de strings en Java](https://www.baeldung.com/java-string-formatter)