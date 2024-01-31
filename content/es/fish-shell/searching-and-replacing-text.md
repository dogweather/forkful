---
title:                "Buscando y reemplazando texto"
date:                  2024-01-20T17:57:40.689330-07:00
model:                 gpt-4-1106-preview
simple_title:         "Buscando y reemplazando texto"

category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Buscar y reemplazar texto es cambiar una cadena de caracteres por otra en un archivo o conjunto de archivos. Los programadores lo usan para corregir errores, actualizar nombres de variables o modificar código eficientemente.

## Cómo Hacerlo:
Para buscar y reemplazar texto en Fish te puedes apoyar en herramientas como `sed`. Aquí un ejemplo rápido:

```Fish Shell
echo "Hola mundo" | sed 's/mundo/Fish/'
```

Salida:
```
Hola Fish
```

Y para cambiar en un archivo:

```Fish Shell
sed -i 's/viejo_texto/nuevo_texto/g' nombre_archivo.txt
```

No olvides que `sed -i` cambia el archivo original. Para solo mostrar la diferencia sin cambiar el archivo, omite la `-i`.

## Profundización
Buscar y reemplazar tiene raíces en los editores de texto como `vi` y `emacs`, perfeccionado con el tiempo. Herramientas como `sed`, `awk`, y `grep` se han vuelto estándares en UNIX. En Fish, se prefieren comandos más legibles y modernos, pero bajo el capó, muchas acciones dependen de estas herramientas antiguas. En términos de alternativas, podrías usar programas modernos como `ripgrep` para buscar, aunque para reemplazar, `sed` sigue siendo muy usado.

La implementación en `sed` usa expresiones regulares, lo que permite reemplazos complejos y no solo texto literal. Esta potencia viene con una curva de aprendizaje, así que vale la pena familiarizarse con las expresiones regulares.

## Ver También
- El manual de Fish en [fishshell.com/docs](https://fishshell.com/docs/current/index.html)
- Una introducción a `sed` en [GNU sed manual](https://www.gnu.org/software/sed/manual/sed.html)
- Para aprender sobre expresiones regulares: [Regular-Expressions.info](https://www.regular-expressions.info/)
