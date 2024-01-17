---
title:                "Borrando caracteres que coinciden con un patrón"
html_title:           "Bash: Borrando caracteres que coinciden con un patrón"
simple_title:         "Borrando caracteres que coinciden con un patrón"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?
Borrar caracteres que coinciden con un patrón es una técnica utilizada por los programadores para eliminar ciertos elementos de un texto o archivo que siguen un patrón en particular. Por ejemplo, se puede utilizar para eliminar caracteres especiales o espacios en blanco de una cadena de texto. Los programadores realizan esta acción para limpiar y formatear datos con el fin de que puedan ser procesados correctamente por sus programas.

## ¿Cómo hacerlo?
Se puede utilizar el comando `sed` en Bash para eliminar caracteres que coinciden con un patrón en un archivo de texto. Por ejemplo, si queremos borrar todas las letras "a" en un archivo llamado `texto.txt`, podemos usar el siguiente comando:
```Bash
sed 's/a//g' texto.txt
```
Esto eliminará todas las letras "a" del archivo y mostrará el resultado en la línea de comandos.

También se puede utilizar el comando `tr` para eliminar caracteres específicos de un archivo. Por ejemplo, si queremos eliminar todas las vocales de un archivo llamado `texto.txt`, podemos usar el siguiente comando:
```Bash
tr -d 'aeiou' < texto.txt
```
Esto eliminará todas las vocales del archivo y mostrará el resultado en la línea de comandos.

## Profundizando
Borrar caracteres que coinciden con un patrón no es una técnica nueva, ya que se ha utilizado desde los primeros días de la programación de computadoras. En los primeros lenguajes de programación, esta acción era realizada mediante el uso de comandos específicos para manipular cadenas de texto. Sin embargo, con el avance de la tecnología, se han creado comandos más eficientes y poderosos, como `sed` y `tr`, que facilitan esta tarea a los programadores.

Además de `sed` y `tr`, también se pueden utilizar otros comandos como `grep` y `awk` para eliminar caracteres que coinciden con un patrón en un archivo. Estos comandos tienen diferentes funcionalidades y opciones, por lo que se recomienda explorarlos para encontrar la mejor solución en cada situación.

En cuanto a la implementación, los comandos `sed` y `tr` utilizan diferentes algoritmos para eliminar caracteres. Mientras que `sed` utiliza expresiones regulares para identificar los patrones, `tr` utiliza tablas de traducción internas. Por lo tanto, dependiendo de la complejidad del patrón, uno puede ser más eficiente que el otro.

## Ver también
- Documentación oficial de `sed`: https://www.gnu.org/software/sed/manual/sed.html
- Documentación oficial de `tr`: https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html
- Documentación oficial de `grep`: https://www.gnu.org/software/grep/manual/grep.html
- Documentación oficial de `awk`: https://www.gnu.org/software/gawk/manual/gawk.html