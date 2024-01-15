---
title:                "Convirtiendo una cadena a minúsculas"
html_title:           "Arduino: Convirtiendo una cadena a minúsculas"
simple_title:         "Convirtiendo una cadena a minúsculas"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## ¿Por qué convertir una cadena a minúsculas en Arduino?

Hay muchas situaciones en las que necesitas trabajar con cadenas de texto en tu proyecto de Arduino. A veces, es posible que quieras comparar dos cadenas o buscar una determinada palabra en una cadena. En tales casos, tener todas las letras en minúsculas puede facilitar la tarea y evitar errores en la comparación. Por lo tanto, aprender a convertir una cadena a minúsculas en Arduino puede ser muy útil.

## Cómo hacerlo

```Arduino
String texto = "ARDUINO";
texto.toLowerCase(); // texto ahora es "arduino"
```

En este ejemplo, declaramos una variable de tipo String llamada "texto" con el valor "ARDUINO". Luego, usamos la función toLowerCase() para convertir toda la cadena a minúsculas. Ahora, si imprimimos la variable "texto", veremos que ha sido modificada a "arduino".

Otra forma de realizar esta conversión es utilizando la función toLowerCase(char) en un bucle. Esta función toma como parámetro un carácter y lo convierte a minúscula. Entonces, podríamos iterar por cada carácter de la cadena y aplicar la función para obtener una cadena totalmente en minúsculas.

## Profundizando

La función toLowerCase() en realidad utiliza la tabla ASCII para convertir los caracteres a minúsculas. Cada letra mayúscula tiene un número asociado en la tabla ASCII, y al sumar 32 a ese número, obtenemos el equivalente en minúscula. Esto podría ser útil si quieres implementar tu propia función para convertir cadenas a minúsculas.

Además, es importante tener en cuenta que la función toLowerCase() solo funciona con letras del alfabeto, por lo que si tienes números u otros símbolos en tu cadena, permanecerán igual. Si deseas convertir toda la cadena a minúsculas, puedes usar la función toLowerCase(char) dentro de un bucle y pasar cada carácter, incluyendo los números y símbolos.

¡Recuerda que siempre puedes consultar la documentación oficial de Arduino para obtener más información sobre esta función y otras!

## Ver también

- [Documentación oficial de Arduino: toLowerCase()](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/tolowercase/) 
- [Tabla ASCII](https://es.wikipedia.org/wiki/ASCII)