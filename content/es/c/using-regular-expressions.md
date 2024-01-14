---
title:                "C: Usando expresiones regulares"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por qué

Las expresiones regulares son una herramienta poderosa en el mundo de la programación. Nos permiten buscar y manipular patrones dentro de un texto, ahorrándonos tiempo y esfuerzo. ¡Sigue leyendo para descubrir cómo utilizarlas en C!

## Cómo usar expresiones regulares en C

Primero, necesitamos incluir la biblioteca estándar `regex.h` en nuestro código. Luego, podemos usar la función `regcomp()` para compilar nuestra expresión regular y la función `regexec()` para ejecutarla en un texto. Por ejemplo:

```C
#include <stdio.h>
#include <regex.h>

int main() {
    char *text = "¡Hola a todos!";
    regex_t regex;
    regmatch_t matches[1];
    
    //compilamos la expresión regular
    regcomp(&regex, "Hola", 0);
    
    //buscamos la coincidencia en el texto
    regexec(&regex, text, 1, matches, 0);
    
    //imprimimos el resultado
    printf("La palabra 'Hola' está en las posiciones %d - %d", matches[0].rm_so, matches[0].rm_eo);
    
    return 0;
}
```

Este código imprimirá "La palabra 'Hola' está en las posiciones 1 - 4", ya que la palabra "Hola" se encuentra en esas posiciones dentro del texto.

## Profundizando en el uso de expresiones regulares

Además de buscar patrones simples, las expresiones regulares nos permiten utilizar métodos más avanzados, como la búsqueda de patrones que se repiten un número específico de veces, la búsqueda de patrones opcionales y la agrupación de patrones. También podemos usar diferentes caracteres especiales, como `^` para denotar que un patrón debe estar al principio del texto, o `$` para denotar que debe estar al final.

Si deseamos una mayor flexibilidad en nuestra búsqueda, podemos usar la función `regcomp()` con el argumento `REG_EXTENDED` para habilitar el uso de sintaxis extendida.

¡Explora y experimenta con diferentes expresiones regulares para descubrir todo lo que puedes lograr con ellas!

## Ver también

- [Expresiones regulares en C - Documentación de GNU](https://www.gnu.org/software/libc/manual/html_node/Regular-Expressions.html)
- [Tutorial de expresiones regulares en C - Jacob Sorber](https://youtu.be/VrT3TRDDE4M)
- [Práctica creando expresiones regulares en C - HackerRank](https://www.hackerrank.com/challenges/valid-regex/problem)