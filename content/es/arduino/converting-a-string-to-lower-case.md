---
title:                "Arduino: Convertir una cadena a minúsculas"
simple_title:         "Convertir una cadena a minúsculas"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## ¿Por qué convertir una cadena a minúsculas?

La conversión de una cadena a minúsculas es una tarea común en la programación de Arduino, especialmente cuando se trabaja con entradas de usuario. Al convertir una cadena a minúsculas, se asegura de que la entrada sea consistente y fácil de manejar en el código.

## Cómo hacerlo

Para convertir una cadena a minúsculas en Arduino, primero debes definir la cadena como una variable de tipo "string". A continuación, puedes utilizar la función `toLowerCase()` para convertir la cadena a minúsculas. A continuación, se muestra un ejemplo de código que convierte una cadena a minúsculas y la imprime en el Monitor Serial:

```arduino
// Definir la cadena
String cadena = "ESTE ES UNA CADENA EN MAYÚSCULAS";

// Convertir la cadena a minúsculas
cadena = cadena.toLowerCase();

// Imprimir la cadena en el Monitor Serial
Serial.println(cadena);
```

El resultado de este código será: "este es una cadena en mayúsculas". Ten en cuenta que esta función solo funciona con letras alfabéticas, por lo que cualquier otro carácter no se verá afectado.

## Profundizando en la conversión

La función `toLowerCase()` utiliza el código ASCII de cada carácter para realizar la conversión. Los códigos ASCII para letras mayúsculas y minúsculas tienen una diferencia de 32, por lo que la función simplemente suma 32 al código ASCII de cada carácter para convertirlo a minúscula.

También es importante tener en cuenta que esta función solo funciona en cadenas de tipo "string". Si se intenta utilizar en un tipo de dato diferente, como un `int` o un `float`, se producirá un error.

## Ver también

- Referencia de funciones de cadena en Arduino: https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/stringtolowercase/
- Códigos ASCII: http://www.asciitable.com