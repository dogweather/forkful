---
title:                "Buscando y reemplazando texto"
html_title:           "Haskell: Buscando y reemplazando texto"
simple_title:         "Buscando y reemplazando texto"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

La búsqueda y reemplazo de texto es una tarea común para los programadores en la computación. Permite la modificación de grandes cantidades de datos de manera eficiente y rápida. Los programadores lo utilizan para actualizar y corregir errores en sus códigos, así como para realizar cambios en bases de datos y otros tipos de archivos.

## ¿Cómo?

A continuación, se muestran algunos ejemplos de cómo realizar la búsqueda y reemplazo de texto en Haskell.

```
-- Cambiar todas las instancias de "perro" a "gato" en una cadena de texto.
reemplazar "perro" "gato" "Quiero un perro como mascota."
> "Quiero un gato como mascota."

-- Cambiar cada palabra en una lista por su longitud.
map length ["hola", "adiós", "gracias"]
> [4, 5, 7]
```

## Profundizando

### Contexto histórico

La búsqueda y reemplazo de texto se remonta a los primeros días de la computación. Los editores de texto como Emacs y Vi han incorporado esta función desde sus inicios. Sin embargo, en lenguajes de programación como Haskell, la búsqueda y reemplazo se puede realizar de manera más eficiente utilizando funciones de orden superior como `map` y `filter`.

### Alternativas

Además de utilizar funciones de orden superior, se pueden utilizar librerías como "Text.Regex" para realizar búsqueda y reemplazo utilizando expresiones regulares. También existen herramientas externas como Grep y Sed que pueden ser utilizadas mediante la integración con Haskell.

### Detalles de implementación

La búsqueda y reemplazo en Haskell se realiza utilizando funciones de manipulación de cadenas de texto, como `replace` y `map`. Estas funciones toman como argumentos la cadena original, el texto a buscar y el texto de reemplazo. Utilizando técnicas de evaluación perezosa, Haskell puede realizar cambios en archivos de gran tamaño de manera eficiente.

## Ver también

- [Documentación de Haskell sobre listas](https://www.haskell.org/tutorial/sets.html)
- [Ejemplos avanzados de búsqueda y reemplazo en Haskell](https://wiki.haskell.org/Regex-TDFA)