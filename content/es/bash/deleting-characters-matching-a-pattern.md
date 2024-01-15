---
title:                "Eliminando caracteres que coinciden con un patrón"
html_title:           "Bash: Eliminando caracteres que coinciden con un patrón"
simple_title:         "Eliminando caracteres que coinciden con un patrón"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por qué

Borrar caracteres que coinciden con un patrón es una tarea común en la programación de Bash. Esto puede ser necesario para limpiar datos o para realizar operaciones específicas en un archivo o cadena.

## Cómo hacerlo

Para borrar caracteres que coinciden con un patrón en Bash, utilizamos el comando `sed` seguido de la expresión regular que queremos encontrar y el archivo o cadena en el que queremos realizar la eliminación.

Por ejemplo, si queremos eliminar todas las letras "a" de un archivo llamado `texto.txt`, usamos el siguiente comando en la terminal:

```
sed 's/a//g' texto.txt
```

Esto eliminará todas las letras "a" del archivo y mostrará el resultado en la terminal. También podemos guardar el resultado en un nuevo archivo agregando el operador de redirección `>` al final del comando.

Si queremos eliminar un patrón específico de un archivo o cadena, podemos usar la opción `-e` seguida de la expresión regular deseada. Por ejemplo, si queremos eliminar todas las palabras que comiencen con la letra "b" de un archivo, usamos el siguiente comando:

```
sed -e 's/\bb[a-z]*//g' archivo.txt
```

Aquí, `\b` representa un límite de palabra y `[a-z]*` representa cualquier cantidad de letras minúsculas. Esto asegura que solo se eliminarán palabras que comiencen con la letra "b". 

## Inmersión profunda

El comando `sed` se utiliza para realizar operaciones de edición de texto en Bash. Su sintaxis general es `sed 'acción' archivo`, donde "acción" puede ser una expresión regular o un comando de edición específico.

Además de la opción `-e`, también podemos usar otras opciones como `-i` para editar un archivo directamente, `-n` para suprimir la salida predeterminada y mostrar solo las líneas que se han modificado, y `-r` para una mayor compatibilidad con expresiones regulares. 

También podemos utilizar diferentes comandos de edición, como `d` para eliminar líneas, `p` para imprimir líneas y `y` para cambiar caracteres. Estos comandos se agregan después de la expresión regular y se separan con una coma.

En resumen, el comando `sed` es una herramienta útil para realizar operaciones de edición de texto en Bash y puede ser personalizado según nuestras necesidades utilizando diferentes opciones y comandos de edición.

## Ver también

- [Documentación de Bash](https://www.gnu.org/software/bash/)
- [Expresiones regulares en Bash](https://www.regular-expressions.info/posix.html)
- [Tutorial de sed](https://www.grymoire.com/Unix/Sed.html)