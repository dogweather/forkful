---
title:                "Capitalizar una cadena"
html_title:           "Fish Shell: Capitalizar una cadena"
simple_title:         "Capitalizar una cadena"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez has necesitado capitalizar una cadena en Fish Shell? Puede que pienses que es difícil o tedioso, pero en realidad es muy simple y puede ser muy útil para mejorar la presentación de tu código.

## Cómo hacerlo

Para capitalizar una cadena en Fish Shell, utilizamos el comando `string capitalize` seguido de la cadena que queremos capitalizar. Por ejemplo:

```
Fish Shell> string capitalize "hola mundo"
Hola mundo
```

Incluso podemos utilizar variables para capitalizar una cadena dinámicamente:

```
Fish Shell> set cadena "bienvenidos al artículo"
Fish Shell> string capitalize $cadena
Bienvenidos al artículo
```

## Profundizando

El comando `string capitalize` utiliza el algoritmo de capitalización de palabras de unicode, lo que significa que toma en cuenta las reglas de capitalización para diferentes idiomas. También se puede personalizar agregando el parámetro `-s` para especificar un delimitador diferente al espacio en blanco, o el parámetro `-m` para indicar que solo se deben capitalizar las primeras letras después del delimitador. Por ejemplo:

```
Fish Shell> string capitalize -s "_" "hola_amigo"
Hola_amigo
Fish Shell> string capitalize -m -s "_" "hola_amigo"
Hola_Amigo
```

## Ver también

- [Página oficial de documentación de Fish Shell](https://fishshell.com/docs/current)
- [Ejemplos de capitalización con Fish Shell](https://fishshell.com/docs/3.3/cmds/string.html#capitalize)