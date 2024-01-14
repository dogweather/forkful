---
title:    "Bash: Borrando caracteres que coinciden con un patrón"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## ¿Por qué borrar caracteres que coinciden con un patrón?

Eliminar caracteres que coinciden con un patrón es una tarea común en la programación de Bash. Puede ser útil para limpiar datos o para realizar transformaciones en cadenas de texto. También puede ser útil para eliminar caracteres no deseados de un archivo. ¡Sigue leyendo para aprender cómo hacerlo!

## Cómo:

```bash
sed -ie '/patron/d' archivo.txt
```

Este comando utiliza la herramienta `sed` para eliminar todas las líneas que coincidan con un patrón especificado del archivo `archivo.txt`. El modificador `-ie` permite aplicar cambios al archivo de forma directa, en lugar de simplemente mostrarlos en la terminal.

Para entender cómo funciona este comando, es necesario conocer un poco de la sintaxis de `sed`. `sed` utiliza expresiones regulares para buscar y manipular texto. En este caso, utilizamos la expresión `/patron/` para buscar todas las líneas que contengan el patrón especificado.

Además del uso de `sed`, también se puede utilizar el comando `tr` para eliminar caracteres que coincidan con un patrón:

```bash
tr -d 'patron' < archivo.txt > archivo_nuevo.txt
```

Este comando utiliza `tr` para eliminar todos los caracteres que coincidan con el patrón especificado del archivo `archivo.txt` y luego guarda el resultado en un nuevo archivo llamado `archivo_nuevo.txt`.

## Profundizando:

`sed` y `tr` también tienen otros usos y opciones que pueden ser útiles para borrar caracteres que coinciden con un patrón. Por ejemplo, se pueden utilizar banderas o modificadores adicionales para realizar cambios específicos, como cambiar la posición de las palabras o reemplazarlos por otros caracteres.

Además, también se pueden utilizar comandos adicionales en conjunto con `sed` o `tr`, como `grep` o `awk`, para crear patrones más complejos y eliminar de manera más eficiente los caracteres deseados.

En resumen, borrar caracteres que coinciden con un patrón es una tarea útil y común en la programación de Bash. Aprender a utilizar las herramientas de `sed` y `tr` para este propósito puede ahorrar tiempo y mejorar la eficiencia en futuros proyectos.

## Ver también:

- Documentación oficial de `sed`: https://www.gnu.org/software/sed/manual/sed.html
- Documentación oficial de `tr`: https://www.gnu.org/software/coreutils/manual/html_node/The-_002d_tr-command.html
- Ejemplos prácticos de uso de `sed` y `tr`: https://www.linuxtechi.com/sed-command-examples-linux-users/