---
title:                "Convirtiendo una cadena a minúsculas"
html_title:           "C: Convirtiendo una cadena a minúsculas"
simple_title:         "Convirtiendo una cadena a minúsculas"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Convertir una cadena de texto a minúsculas es un proceso común en la programación. Consiste en transformar todas las letras mayúsculas de una palabra o frase a su equivalente en minúsculas. Los programadores lo hacen para asegurarse de que los datos que reciben sean uniformes y puedan ser comparados correctamente.

## Cómo:

```
#include <stdio.h>
#include <string.h>

int main() {

  char str[] = "PROGRAMACIÓN ES DIVERTIDO";
  int i;

  printf("Cadena original: %s\n", str);

  for(i = 0; str[i]; i++) {
    str[i] = tolower(str[i]);
  }

  printf("Cadena convertida a minúsculas: %s\n", str);

  return 0;
}
```

La salida de este programa sería:

```
Cadena original: PROGRAMACIÓN ES DIVERTIDO
Cadena convertida a minúsculas: programación es divertido
```

## Detalles más profundos:

Con la creciente popularidad de los lenguajes de programación que son sensibles a mayúsculas y minúsculas, como Python o JavaScript, la conversión a minúsculas se ha vuelto aún más importante. En el pasado, los programadores solían utilizar funciones como `toupper()` y `tolower()` para convertir manualmente cada carácter, pero ahora hay funciones específicas para la conversión de cadenas en la mayoría de los lenguajes, incluyendo C.

En algunos casos, se puede utilizar una función de comparación que ignore las diferencias entre mayúsculas y minúsculas en lugar de convertir toda una cadena a minúsculas. Esto puede ser útil cuando se desea comparar cadenas sin importar si están escritas en mayúsculas o minúsculas.

La implementación de la conversión de cadenas a minúsculas puede variar según el lenguaje y la plataforma utilizados. En C, por ejemplo, se puede utilizar la función `tolower()` de la biblioteca estándar `string.h`, como se muestra en el ejemplo anterior.

## Ver también:

- [Función tolower() en la documentación de C](https://www.tutorialspoint.com/c_standard_library/c_function_tolower.htm)
- [Alternativas para la conversión de cadenas en otros lenguajes, como Python o JavaScript](https://www.programiz.com/python-programming/methods/string/startswith)
- [Los beneficios de utilizar lenguajes de programación sin sensibilidad a mayúsculas y minúsculas](https://www.lifewire.com/case-sensitivity-in-computer-programming-373292)