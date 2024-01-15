---
title:                "Buscando y reemplazando texto"
html_title:           "C: Buscando y reemplazando texto"
simple_title:         "Buscando y reemplazando texto"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## ¿Por qué buscar y reemplazar texto en C?

Una de las principales razones por las que se busca y reemplaza texto en C es para realizar cambios o correcciones en un programa de manera rápida y eficiente. También puede ser útil para hacer cambios en múltiples líneas de código simultáneamente.

## Cómo hacerlo

Para buscar y reemplazar texto en C, podemos utilizar la función `strstr()` de la biblioteca estándar de C. Esta función busca la primera aparición de una cadena dentro de otra cadena y devuelve un puntero a esa posición en la cadena original.

Por ejemplo, si queremos reemplazar todas las apariciones de la palabra "hola" por "adiós" en una cadena llamada `mensaje`, podemos usar el siguiente código:

```C
char* nuevo_mensaje = strstr(mensaje, "hola");
while (nuevo_mensaje != NULL) {
    strncpy(nuevo_mensaje, "adiós", 5);
    nuevo_mensaje = strstr(nuevo_mensaje + 5, "hola");
}
```

El `while` loop nos permite reemplazar todas las apariciones de "hola" en `mensaje`. El primer parámetro de `strstr` es la cadena original y el segundo es la cadena que estamos buscando. Luego, utilizamos `strncpy` para reemplazar la cadena encontrada con nuestra nueva cadena.

## Un poco más profundo

La función `strstr` no solo es útil para buscar y reemplazar cadenas de texto en C, sino que también puede ser utilizada para otras operaciones, como dividir una cadena en varias subcadenas.

También es importante tener en cuenta que `strstr` es sensible a mayúsculas y minúsculas, por lo que si queremos buscar y reemplazar de manera insensible a mayúsculas y minúsculas, podemos utilizar la función `strcasestr` de la biblioteca `strings.h`.

## Véase también

- [Documentación de la función strstr en C](https://www.tutorialspoint.com/c_standard_library/c_function_strstr.htm)
- [Ejemplo de búsqueda y reemplazo de texto en C](https://www.geeksforgeeks.org/c-program-replace-word-text-another-given-source-string/)
- [Otras funciones útiles para manipular cadenas en C](https://www.geeksforgeeks.org/string-manipulation-in-c-without-using-in-built-function/)