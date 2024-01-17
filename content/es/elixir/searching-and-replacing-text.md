---
title:                "Buscando y reemplazando texto"
html_title:           "Elixir: Buscando y reemplazando texto"
simple_title:         "Buscando y reemplazando texto"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

¡Hola programadores! ¿Alguna vez te has encontrado con la tarea de buscar y reemplazar texto en tu código? Si es así, ¡estás en el lugar correcto! En este artículo, exploraremos cómo hacerlo en Elixir de forma fácil y eficiente.

## ¿Qué y por qué?
Buscar y reemplazar es una técnica común utilizada por los programadores para encontrar y cambiar texto específico en un archivo o código. Esto puede ser útil para corregir errores, actualizar versiones o simplemente para hacer cambios en una gran cantidad de líneas de código de una sola vez.

## Cómo hacerlo:
En Elixir, buscar y reemplazar se puede lograr fácilmente utilizando la función `String.replace/3`. Esta función toma tres argumentos: la cadena original, la cadena a buscar y la cadena de reemplazo. Veamos un ejemplo:

```Elixir
original = "Hola mundo!"
buscar = "mundo"
reemplazo = "amigos"

String.replace(original, buscar, reemplazo)

# Salida: "Hola amigos!"
```

Podemos ver que la función reemplazó la palabra "mundo" por "amigos" en la cadena original.

## Profundizando:
La búsqueda y reemplazo de texto no es un concepto nuevo en la programación. De hecho, se ha utilizado durante décadas en diferentes lenguajes de programación. Algunos de estos lenguajes, como Perl y Sed, tienen funciones específicas para realizar esta tarea.

Si bien la función `String.replace/3` en Elixir es muy útil, también existe la opción de utilizar expresiones regulares para realizar la búsqueda y el reemplazo de texto. Las expresiones regulares son patrones utilizados para encontrar coincidencias en cadenas de texto. Pueden ser muy poderosas en la manipulación de cadenas de texto, pero pueden ser un poco más complejas de aprender y utilizar en comparación con la función `String.replace/3`.

En cuanto a la implementación interna de la búsqueda y reemplazo de texto en Elixir, la función `String.replace/3` utiliza la biblioteca de PCRE (Perl Compatible Regular Expressions) detrás de escena para realizar la búsqueda y el reemplazo.

## Ver también:
Si quieres aprender más sobre expresiones regulares en Elixir, puedes revisar la documentación oficial de Elixir sobre el tema. También puedes explorar la biblioteca PCRE en sí para ver más opciones y funcionalidades.

Espero que este artículo te haya sido útil para entender cómo buscar y reemplazar texto en Elixir. ¡Ahora puedes aplicar esta técnica en tus propios proyectos! Hasta la próxima, ¡feliz programación!