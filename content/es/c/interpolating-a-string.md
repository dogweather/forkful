---
title:                "Interpolando una cadena"
html_title:           "C: Interpolando una cadena"
simple_title:         "Interpolando una cadena"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/interpolating-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Interpolar una cadena de texto en programación se refiere a insertar valores de variables dentro de una cadena de texto para crear una cadena de texto final dinámica. Los programadores lo hacen para evitar escribir múltiples cadenas de texto con valores variables y para hacer su código más legible.

## Cómo:
Un ejemplo básico de interpolación de cadenas en C es el siguiente:

```C
int age = 26;
char* name = "John";

printf("Hola, mi nombre es %s y tengo %d años.", name, age);
```

El resultado en la consola sería:

```
Hola, mi nombre es John y tengo 26 años.
```

También es posible formatear la salida usando especificadores de formato, por ejemplo:

```C
double price = 9.99;

printf("El precio es $%.2lf.", price);
```

El resultado sería:

```
El precio es $9.99.
```

## Profundizando:
La interpolación de cadenas en C se introdujo en la versión inicial del lenguaje en 1972. En ese entonces, era una característica novedosa que facilitaba la creación de cadenas de texto dinámicas. Sin embargo, con el tiempo, han surgido otras formas de lograr el mismo resultado, como el concatenado de cadenas o el uso de funciones de formateo.

## Ver también:
Para más información sobre lenguaje C y sus características, puedes consultar la documentación oficial de [C](https://www.cprogramming.com/tutorial/c-tutorial.html). También puedes aprender sobre el uso de especificadores de formato en [este artículo](https://www.techonthenet.com/c_language/standard_library_functions/printf.php).