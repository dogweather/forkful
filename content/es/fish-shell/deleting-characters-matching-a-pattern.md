---
title:                "Eliminando caracteres que coinciden con un patrón"
html_title:           "Elixir: Eliminando caracteres que coinciden con un patrón"
simple_title:         "Eliminando caracteres que coinciden con un patrón"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## ¿Qué es y por qué?
Eliminar caracteres que coinciden con un patrón significa borrar todos los caracteres que se ajustan a cierto criterio en un fragmento de código. Los programadores lo hacen para modelar los datos, eliminar errores o simplificar el texto.

## Cómo hacerlo:
Aquí te muestro cómo puedes hacerlo en Fish Shell:

```Fish Shell
set cadena "Hola, Mundo!"
echo $cadena | string replace -r "," ""
```

La salida será:

```Fish Shell
Hola Mundo!
```

En este ejemplo, hemos eliminado todas las comas de la cadena.

## Análisis Profundo:
Eliminar caracteres que coinciden con un patrón en Fish Shell no tiene un linaje histórico especial, pero es un concepto común de procesamiento de texto en la mayoría de los lenguajes de programación. 

Como alternativa, puedes usar el comando `tr` en shells que no son de Fish, así:

```Bash
echo "Hola, Mundo!" | tr -d ","
```

Tanto Fish Shell como Bash implementan esta característica mediante una búsqueda lineal en la cadena para identificar y retirar los caracteres. No hay una complejidad significativa en este proceso ya que se lleva a cabo en tiempo lineal.

## Ver También:
1. Documentación oficial de `string` en Fish Shell: [https://fishshell.com/docs/current/commands.html#string](https://fishshell.com/docs/current/commands.html#string)
2. Tutorial sobre `tr` en Bash: [https://linuxhint.com/bash_tr_command/](https://linuxhint.com/bash_tr_command/)
3. Un análisis interno detallado de cómo Fish Shell procesa las cadenas: [https://github.com/fish-shell/fish-shell/issues/563](https://github.com/fish-shell/fish-shell/issues/563)