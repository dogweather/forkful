---
title:                "Encontrando la longitud de una cadena"
html_title:           "Arduino: Encontrando la longitud de una cadena"
simple_title:         "Encontrando la longitud de una cadena"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Encontrar la Longitud de una Cadena en C

## ¿Qué y Por qué?

Encontrar la longitud de una cadena significa contabilizar los caracteres de una cadena de texto. Los programadores lo usan para manipular textos eficientemente, evitando errores y aumentando la velocidad de sus programas.

## Cómo hacerlo:

Definamos una función que recoja la longitud de una cadena en el lenguaje C. En el siguiente bloque de código se encuentra un ejemplo de implementación:

```C
#include <stdio.h>
#include <string.h>

int main() {
    char cadena[] = "Hola Mundo";
    int longitud = strlen(cadena);
    printf("La longitud de '%s' es %d", cadena, longitud);
    return 0;
}
```

Cuando ejecutes este código, verás la salida:

```
La longitud de 'Hola Mundo' es 10
```

## Buceo Profundo

En el contexto histórico, el lenguaje C original no proveía un medio directo para conocer la longitud de una cadena. Esto resultó en diversas funciones, principalmente 'strlen', que se convirtieron en estándares de facto.

Existen también métodos alternativos para encontrar la longitud de una cadena, como recorrer la cadena desde el principio hasta encontrar el carácter de fin de cadena (`'\0'`), pero generalmente son menos eficientes.

Las implementaciones de `strlen` son muy importantes. La mayoría de las implementaciones de `strlen` en las librerías de C exitosas son extremadamente rápidas, a menudo sacando provecho de las instrucciones del procesador específicas para la búsqueda de cadenas y procesamiento paralelo.

## Ver También

Para más información, consulte los siguientes link:

1. [Documentación oficial de `strlen`](https://www.cplusplus.com/reference/cstring/strlen/)
2. [Discusión en StackOverflow sobre cómo trabaja `strlen`](https://stackoverflow.com/questions/308695/how-do-the-c-and-c-string-functions-work-internally)
3. [Tutorial sobre cadenas en C](https://www.tutorialspoint.com/cprogramming/c_strings.htm)