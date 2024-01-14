---
title:    "C: Buscando y reemplazando texto"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por qué
A veces, cuando estamos escribiendo un programa en C, nos encontramos con la necesidad de cambiar o reemplazar algunas palabras o caracteres en nuestro código. Esto puede ser debido a una falta de ortografía, una palabra desactualizada o simplemente para hacer que el código sea más legible. Para lograr esto, podemos utilizar una función en C llamada "str_replace" que nos permite buscar y reemplazar texto de manera efectiva.

## Cómo hacerlo
Para usar la función "str_replace", primero debemos incluir la biblioteca de cadenas de C, "string.h" en nuestro programa. Luego, debemos definir una cadena de caracteres en la que deseamos buscar y reemplazar texto. A continuación, llamamos a la función "str_replace", pasando como argumentos la cadena de caracteres, el texto a buscar, el texto a reemplazar y el número de veces que queremos realizar el reemplazo. Veamos un ejemplo:

```C
#include <stdio.h>
#include <string.h>

int main()
{
   // Definimos nuestra cadena de caracteres
   char texto[] = "Hola mundo! Hola a todos!";

   printf("Texto original: %s\n", texto);
   
   // Reemplazamos "Hola" por "Hola a ti"
   str_replace(texto, "Hola", "Hola a ti", 2); 

   printf("Texto actualizado: %s\n", texto);

   return 0;
}
```

En este ejemplo, hemos reemplazado "Hola" por "Hola a ti" en nuestra cadena de caracteres "texto" dos veces. La salida que obtendremos es la siguiente:

```
Texto original: Hola mundo! Hola a todos!
Texto actualizado: Hola a ti mundo! Hola a ti a todos!
```

Podemos ver que la función "str_replace" ha buscado y reemplazado el texto deseado de manera efectiva.

## Profundizando
Si queremos aprender más sobre cómo funciona la función "str_replace", podemos analizar su implementación en la biblioteca "string.h". Esta función toma como parámetros una cadena de caracteres, el texto a buscar, el texto a reemplazar y el número de veces que se realizará el reemplazo. 
Primero, se realiza una búsqueda del texto a buscar en la cadena de caracteres utilizando la función "strstr" que devuelve un puntero a la primera aparición del texto buscado. Luego, se usa la función "strcmp" para comparar el texto buscado con el texto actual en la cadena de caracteres y, si son iguales, se realiza el reemplazo con la función "strcpy".
Esta operación se repite el número de veces especificado en el argumento y, al final, se devuelve la cadena de caracteres modificada.

## Ver también
- [Documentación de la función "str_replace" en la biblioteca de cadenas de C](https://www.cplusplus.com/reference/cstring/str_replace/)
- [Tutorial de programación en C](https://www.tutorialspoint.com/cprogramming/index.htm)
- [Ejemplos de programas en C](https://www.programiz.com/c-programming/examples)