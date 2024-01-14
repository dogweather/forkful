---
title:                "C: Borrado de caracteres que coinciden con un patrón."
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## ¿Por qué borrar caracteres que coincidan con un patrón?

A veces en la programación, nos encontramos con la necesidad de eliminar caracteres de una cadena que coincidan con un patrón específico. Esto puede ser útil para limpiar datos o para validar la entrada del usuario. En este artículo, exploraremos cómo podemos lograr esto en lenguaje C.

## Cómo hacerlo

Para eliminar caracteres coincidentes con un patrón en C, utilizaremos la función `memmove()` de la librería `string.h`. Esta función nos permite mover cierta cantidad de bytes de una posición de memoria a otra. En nuestro caso, vamos a usarla para mover los caracteres que queremos eliminar al final de la cadena.

El siguiente código es un ejemplo de cómo podemos eliminar los caracteres "a" y "b" de la cadena "holaab":

```C
#include <stdio.h>
#include <string.h>

void deleteCharacters(char *string, char *pattern){
    char *match;
    while ((match = strstr(string, pattern)) != NULL) {
        memmove(match, match + strlen(pattern), strlen(string) + 1 - strlen(pattern));
    }
}

int main(){
    char name[10] = "holaab";
    char pattern[] = "ab";
    deleteCharacters(name, pattern);

    printf("%s", name); //imprimirá "hol"
    return 0;
}
```

En este ejemplo, la función `deleteCharacters()` recibe dos argumentos: un puntero a la cadena donde queremos eliminar los caracteres y un puntero al patrón que queremos borrar. Dentro de la función, utilizamos un bucle `while` y la función `strstr()` para encontrar la posición de la primera coincidencia del patrón en la cadena. Luego, utilizando `memmove()`, movemos los caracteres que están después de la coincidencia al inicio de la cadena, sobreescribiendo así los caracteres que queremos eliminar. Este proceso se repite hasta que ya no hay coincidencias.

## Profundizando

Ahora que conocemos la función `memmove()` y cómo podemos utilizarla para eliminar caracteres coincidentes con un patrón, es importante tener en cuenta algunas cosas:

- La función `memmove()` trabaja con posiciones de memoria, no con caracteres específicos. Por lo tanto, siempre debemos tener cuidado de no sobrepasar los límites de nuestra cadena al mover los caracteres.
- Si queremos eliminar un solo carácter, podemos simplemente mover un solo byte en lugar de toda una cadena.
- Podemos adaptar este enfoque para eliminar más de un patrón en una sola cadena, simplemente llamando a `deleteCharacters()` varias veces.

¡Ahora ya sabes cómo eliminar caracteres coincidentes con un patrón en C! Si tienes dudas o quieres seguir aprendiendo sobre este tema, te recomendamos estos recursos adicionales:

## Vea también

- [Documentación de la función `memmove()` en cplusplus.com](https://www.cplusplus.com/reference/cstring/memmove/)
- [Tutorial de C en el canal de Youtube Programación ATS](https://www.youtube.com/watch?v=RURck40-v1k)