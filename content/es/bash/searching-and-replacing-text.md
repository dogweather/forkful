---
title:                "Bash: Buscar y reemplazar texto"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por qué
Buscar y reemplazar texto es una tarea muy común en la programación de Bash. Es útil cuando necesitamos hacer cambios rápidos y automáticos en grandes cantidades de texto, ahorrando tiempo y esfuerzo.

## Cómo hacerlo
Podemos usar el comando `sed` en Bash para buscar y reemplazar texto en un archivo. Veamos un ejemplo:

```
sed -i 's/antiguo/nuevo/g' archivo.txt
```

En este comando, `sed` busca el texto "antiguo" en el archivo y lo reemplaza por "nuevo". La opción `-i` hace que los cambios se realicen directamente en el archivo, en lugar de mostrarlos en la salida estándar. El `/g` al final del comando asegura que se reemplacen todas las ocurrencias del texto en cada línea.

También podemos usar expresiones regulares para buscar y reemplazar patrones más complejos. Por ejemplo, si queremos reemplazar todas las letras mayúsculas en un archivo con su versión en minúsculas, podemos usar este comando:

```
sed -i 's/[A-Z]/\L&/g' archivo.txt
```

En este caso, `\L` indica que se convierta el texto a minúsculas y `&` representa el texto que se encuentra en la búsqueda.

## Profundizando
El comando `sed` es muy poderoso y tiene varias opciones y argumentos que pueden ser combinados para realizar búsquedas y reemplazos más específicos. Por ejemplo, podemos usar el parámetro `c` para cambiar una línea completa por un nuevo texto, o `y` para cambiar un conjunto de caracteres por otro.

Además, podemos combinar `sed` con otros comandos y utilizar pipes (`|`) para realizar búsquedas y reemplazos en la salida de un comando anterior. Por ejemplo, si queremos reemplazar todos los espacios en blanco por un guión bajo en la salida del comando `ls`, podemos hacer lo siguiente:

```
ls | sed 's/ /_/g'
```

Otra opción útil es el uso de variables para almacenar los textos a buscar y reemplazar. De esta manera, podemos automatizar procesos y hacer búsquedas y reemplazos en múltiples archivos sin tener que escribir el texto cada vez.

## Ver También
- [Documentación de `sed`](https://www.gnu.org/software/sed/manual/sed.html)
- [Guía de expresiones regulares en Bash](https://www.markdownguide.org/basic-syntax/)