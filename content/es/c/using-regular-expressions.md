---
title:                "C: Utilizando expresiones regulares"
simple_title:         "Utilizando expresiones regulares"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por qué utilizar expresiones regulares en programación

Las expresiones regulares son una herramienta poderosa en la programación que nos permiten buscar y manipular patrones de texto de manera eficiente. Son especialmente útiles cuando se desea validar o procesar grandes cantidades de información en un programa.

## Cómo utilizar expresiones regulares en C

En C, podemos utilizar expresiones regulares a través de la biblioteca estándar <regex.h>. Primero, debemos declarar una variable del tipo "regex_t" que almacene nuestra expresión regular y luego compilarla con la función "regcomp()". A continuación, podemos utilizar la función "regexec()" para comparar nuestra expresión con una cadena de texto y obtener resultados como el número de coincidencias y su posición.

```C
#include <stdio.h>
#include <regex.h>

int main(void) 
{
  // Declarar e inicializar variables
  regex_t regex;
  char str[] = "¡Hola! Mi nombre es Juan.";
  int reti;

  // Compilar la expresión regular
  reti = regcomp(&regex, "Hola.*Juan", 0);
  if (reti) 
  {
    printf("No se pudo compilar la expresión regular\n");
    exit(1);
  }

  // Comparar la expresión con la cadena de texto
  reti = regexec(&regex, str, 0, NULL, 0);
  if (!reti) 
  {
    printf("¡La expresión coincide con la cadena de texto!\n");
  } 
  else if (reti == REG_NOMATCH) 
  {
    printf("La expresión no coincide con la cadena de texto\n");
  } 
  else 
  {
    printf("Error al ejecutar la expresión regular\n");
    exit(1);
  }

  // Liberar memoria y finalizar programa
  regfree(&regex);

  return 0;
}
```

La salida de este programa sería:
"¡La expresión coincide con la cadena de texto!"

## Una mirada más profunda a las expresiones regulares en C

Además de las funciones mencionadas anteriormente, la biblioteca <regex.h> también nos proporciona una serie de funciones para realizar operaciones más complejas con expresiones regulares, como repeticiones, agrupaciones y búsqueda de subcadenas. También es importante tener en cuenta que esta biblioteca utiliza la sintaxis POSIX para expresiones regulares, lo que puede diferir de otras implementaciones que hayamos utilizado previamente.

En general, es recomendable consultar la documentación oficial para familiarizarse con todas las funciones disponibles y sus respectivos parámetros. También es útil practicar con diferentes ejemplos para comprender mejor el funcionamiento de las expresiones regulares en C.

## Ver también

- [Guía de expresiones regulares en C](https://www.gnu.org/software/libc/manual/html_node/Regular-Expressions-in-C.html)
- [Documentación de <regex.h> en CPlusPlus](https://www.cplusplus.com/reference/regex/)
- [Tutorial de Regex en C en Programando paso a paso](https://programando.pasosdevelocidad.com/2020/08/expresiones-regulares-en-c-con-un-ejemplo-practico/)