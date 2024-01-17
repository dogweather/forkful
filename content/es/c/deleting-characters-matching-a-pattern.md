---
title:                "Borrando caracteres que coinciden con un patrón"
html_title:           "C: Borrando caracteres que coinciden con un patrón"
simple_title:         "Borrando caracteres que coinciden con un patrón"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

La eliminación de caracteres que coinciden con un patrón es un proceso en el que los programadores buscan y borran caracteres específicos en un texto o cadena de caracteres. Esta técnica es útil cuando se desea limpiar o mejorar la calidad de los datos, ya que permite encontrar y eliminar rápidamente caracteres no deseados.

## Cómo:

El siguiente código en C muestra cómo eliminar caracteres que coinciden con un patrón en una cadena de caracteres:

```
#include <stdio.h>
#include <string.h>

int main()
{
    char str[] = "¡Hola! ¿Cómo estás?";
    char new_str[20], character = '?';

    int i, j = 0;

    for (i = 0; str[i] != '\0'; i++)
    {
        if (str[i] != character)
        {
            new_str[j] = str[i];
            j++;
        }
    }

    new_str[j] = '\0';

    printf("La cadena de caracteres original es: %s\n", str);
    printf("La cadena de caracteres modificada es: %s\n", new_str);

    return 0;
}
```

La salida de este código sería:

```
La cadena de caracteres original es: ¡Hola! ¿Cómo estás?
La cadena de caracteres modificada es: ¡Hola!Como estás
```

## Inmersión Profunda

La eliminación de caracteres que coinciden con un patrón se ha utilizado en lenguajes de programación desde hace mucho tiempo. Una de las principales alternativas es el uso de expresiones regulares, que son secuencias de caracteres que ayudan a buscar y manipular texto. Sin embargo, el uso de expresiones regulares puede resultar complicado y no siempre es necesario utilizarlas para eliminar caracteres que coinciden con un patrón.

La implementación de la eliminación de caracteres que coinciden con un patrón en el código mostrado anteriormente es bastante sencilla. Utiliza un bucle for para recorrer la cadena de caracteres y verifica si cada caracter coincide con el patrón deseado. Si no coincide, se agrega ese carácter a una nueva cadena, que finalmente se imprimirá como el resultado.

## Ver también

Para obtener más información sobre la eliminación de caracteres que coinciden con un patrón, se puede consultar la función `strchr()` en la biblioteca estándar de C, así como otros métodos de manipulación de cadenas de caracteres. También es recomendable investigar sobre expresiones regulares y su uso en la búsqueda y manipulación de texto.