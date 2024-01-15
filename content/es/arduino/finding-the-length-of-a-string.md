---
title:                "Encontrando la longitud de una cadena"
html_title:           "Arduino: Encontrando la longitud de una cadena"
simple_title:         "Encontrando la longitud de una cadena"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## ¿Por qué?

¿Alguna vez has necesitado saber la longitud de una cadena de texto en tu programa de Arduino? Puede parecer un concepto simple, pero es una habilidad útil y básica en la programación.

## Cómo:

Para encontrar la longitud de una cadena de texto en Arduino, podemos usar la función `strlen ()`. Esta función cuenta el número de caracteres en una cadena y devuelve el resultado como un entero.

```Arduino
// declaración de la cadena de texto y variable
char palabra[] = "hola";
int longitud;

// uso de la función strlen ()
longitud = strlen(palabra);

// impresión del resultado en el monitor serial
Serial.println(longitud);

// resultado: 4
```

¡Fácil, verdad? También podemos usar la función `sizeof()` para encontrar la longitud de una cadena, pero esto incluirá el espacio adicional utilizado para almacenar la cadena en la memoria.

```Arduino
// declaración de la cadena de texto
char palabra[] = "hola";

// uso de la función sizeof()
int longitud = sizeof(palabra);

// impresión del resultado en el monitor serial
Serial.println(longitud);

// resultado: 5
```

## Inmersión profunda:
Ahora que sabemos cómo encontrar la longitud de una cadena en Arduino, veamos cómo funciona la función `strlen ()` en detalle. En realidad, esta función recorre cada carácter de la cadena y los cuenta hasta encontrar el último carácter `null` (0) que indica el final de la cadena. Es por eso que el resultado de `strlen()` no incluye este último carácter.

También es importante tener en cuenta que esta función solo funciona con cadenas de texto y no con otros tipos de datos, como enteros o booleanos.

## Ver también:
- [Función strlen() en la documentación de Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/strlen/)
- [Función sizeof() en la documentación de Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/sizeof/)
- [Explicación detallada sobre cadenas de texto en Arduino](https://www.electronicshub.org/arduino-string/)