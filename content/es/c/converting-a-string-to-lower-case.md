---
title:                "C: Convirtiendo una cadena a minúsculas"
simple_title:         "Convirtiendo una cadena a minúsculas"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por qué

Convertir una cadena de texto a minúsculas es una tarea común en la programación en C. Esto puede ser útil al realizar operaciones de comparación de cadenas o al formatear la salida de datos. También puede mejorar la legibilidad del código para aquellos que estén acostumbrados a trabajar con cadenas en minúsculas.

## Cómo hacerlo

Para convertir una cadena de texto a minúsculas en C, existen varias formas de lograrlo. Una de ellas es utilizando la función `tolower()` que se encuentra en la biblioteca estándar `ctype.h`. Esta función toma un carácter como argumento y devuelve su versión en minúsculas.

```C
#include <stdio.h>
#include <ctype.h>

int main()
{
    char letra = 'A';
    printf("Letra original: %c\n", letra);
    letra = tolower(letra);
    printf("Letra convertida: %c\n", letra);
    return 0;
}
```

Output:

```
Letra original: A
Letra convertida: a
```

Si deseamos convertir una cadena completa a minúsculas, podemos utilizar una función personalizada con un bucle `for` que recorra cada carácter y utilice `tolower()` en cada uno de ellos.

```C
#include <stdio.h>
#include <string.h>
#include <ctype.h>

void minusculas(char cadena[])
{
    for(int i = 0; i < strlen(cadena); i++)
    {
        cadena[i] = tolower(cadena[i]);
    }
}

int main()
{
    char cadena[] = "Hola Mundo";
    printf("Cadena original: %s\n", cadena);
    minusculas(cadena);
    printf("Cadena convertida: %s\n", cadena);
    return 0;
}
```

Output:

```
Cadena original: Hola Mundo
Cadena convertida: hola mundo
```

## Profundizando en el tema

Si bien las dos formas mencionadas cumplen con el objetivo de convertir una cadena de texto a minúsculas, existen algunas diferencias que vale la pena mencionar. La función `tolower()` solo funciona con caracteres individuales, por lo que si deseamos convertir una cadena que contenga números o símbolos, estos serán ignorados y permanecerán sin cambios.

Por otro lado, al utilizar una función personalizada con un bucle `for`, podemos incluir condiciones para que también se conviertan caracteres especiales. También podemos modificar la función para que solo convierta ciertos caracteres específicos, según nuestras necesidades.

Además, es importante tener en cuenta que al trabajar con cadenas en C, debemos asegurarnos de tener suficiente memoria asignada para almacenar la cadena resultante luego de la conversión. De lo contrario, podríamos tener problemas de desbordamiento de búfer.

## Ver también

- [Documentación de la función `tolower()` en C](https://www.tutorialspoint.com/c_standard_library/c_function_tolower.htm)
- [Tutorial para trabajar con cadenas en C](https://www.programiz.com/c-programming/c-strings)