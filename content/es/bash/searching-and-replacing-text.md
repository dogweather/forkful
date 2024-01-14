---
title:    "Bash: Buscar y reemplazar texto"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## ¿Por qué?

La búsqueda y reemplazo de texto es una habilidad importante para cualquier programador de Bash. Te permite ahorrar tiempo y esfuerzo al realizar cambios en grandes cantidades de texto de manera rápida y eficiente. También es útil para corregir errores ortográficos o cambios en el formato en múltiples archivos al mismo tiempo.

## ¿Cómo hacerlo?

La sintaxis básica para buscar y reemplazar en Bash es la siguiente:

```Bash
sed -i 's/búsqueda/reemplazo/g' archivo.txt
```

Aquí, "sed" es el comando utilizado para buscar y reemplazar, "-i" indica que se modificará directamente el archivo, 's/búsqueda/reemplazo/g' es la expresión regular que indica qué texto debe ser reemplazado y "archivo.txt" es el nombre del archivo en el que se llevará a cabo la búsqueda y reemplazo.

Por ejemplo, si queremos reemplazar todas las apariciones de la palabra "hola" por "adiós" en un archivo llamado "saludos.txt", el comando se vería así:

```Bash
sed -i 's/hola/adiós/g' saludos.txt
```

El comando anterior buscará y reemplazará todas las apariciones de "hola" por "adiós" en el archivo "saludos.txt".

## Profundizando

La búsqueda y reemplazo en Bash también permite buscar y reemplazar patrones de texto más complejos utilizando expresiones regulares. Por ejemplo, si queremos reemplazar todas las apariciones de una palabra que comience con la letra "a", podemos utilizar la expresión regular 's/a[a-z]*/nuevo-texto/g'. Esto buscará y reemplazará todas las palabras que comiencen con "a" seguidas de cualquier otra letra o letras con el texto "nuevo-texto" en su lugar.

También podemos utilizar la opción "-r" para que Bash interprete las expresiones regulares de manera extendida, lo que nos permite utilizar características más avanzadas como grupos y cuantificadores.

Es importante tener en cuenta que la búsqueda y reemplazo en Bash es sensible a mayúsculas y minúsculas. Para realizar una búsqueda sin distinguir entre mayúsculas y minúsculas, podemos utilizar la opción "-i".

## Ver también

- Tutorial de expresiones regulares en Bash: https://www.gnu.org/software/sed/manual/html_node/Regular-Expressions.html
- Tutorial de búsqueda y reemplazo en Bash: http://tldp.org/LDP/abs/html/textproc.html#SEDREF
- Ejemplos prácticos de búsqueda y reemplazo en Bash: https://www.computerhope.com/unix/used.htm#sed

¡Ahora tú puedes utilizar la búsqueda y reemplazo en Bash para ahorrar tiempo y esfuerzo en tus proyectos! Recuerda siempre revisar y probar tus expresiones regulares antes de aplicarlas a grandes cantidades de texto. ¡Feliz programación!