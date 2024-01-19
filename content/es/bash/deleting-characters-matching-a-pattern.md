---
title:                "Eliminando caracteres que coinciden con un patrón"
html_title:           "Elixir: Eliminando caracteres que coinciden con un patrón"
simple_title:         "Eliminando caracteres que coinciden con un patrón"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Eliminar caracteres que coinciden con un patrón es una técnica muy útil en programación. Permite a los programadores gestionar strings y hacer que su código sea más eficiente y legible.

## Cómo hacerlo:

Aquí hay un ejemplo en Bash para borrar caracteres que coinciden con un patrón.

```Bash
$ string="HolaMundo!"
$ echo ${string//O/A}
HolaMundo!
```

Como puedes ver, hemos reemplazado todas las ocurrencias de 'O' con 'A' en la cadena original.

## Análisis Detallado

Eliminar caracteres que coinciden con un patrón tiene unas raíces históricas profundas en Unix y las shell de línea de comando. Bash, nacido en 1989, soporta esta funcionalidad nativamente.

Hay varias alternativas para hacer esto en Bash, como el uso de `tr`, `sed` o `awk`, cada uno con sus propias ventajas y desventajas.

```Bash
$ echo "HolaMundo!" | tr 'O' 'A'
HolaMundo!
```
```Bash
$ echo "HolaMundo!" | sed 's/O/A/g'
HolaMundo!
```
```Bash
$ echo "HolaMundo!" | awk '{$0=gsub(/O/, "A") print $0}'
HolaMundo!
```

Cuando usas `${string//O/A}`, Bash busca internamente a través de todos los caracteres en `string`, comparando cada carácter con 'O'. Si encuentra un match, lo reemplaza con 'A'.

## Ver También

Para más detalles y utilidades, echa un vistazo a estos recursos:

* [Expresiones de parámetro en Bash](https://tldp.org/LDP/abs/html/parameter-substitution.html)
* [Manejo de strings en Bash](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html)
* [Comando `tr` en Unix/Linux](https://www.geeksforgeeks.org/tr-command-unixlinux-examples/)
* [Comando `sed` en Unix/Linux](https://www.geeksforgeeks.org/sed-command-in-linux-unix-with-examples/)
* [Comando `awk` en Unix/Linux](https://www.geeksforgeeks.org/awk-command-unixlinux-examples/)