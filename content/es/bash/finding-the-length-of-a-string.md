---
title:                "Encontrando la longitud de una cadena"
html_title:           "Bash: Encontrando la longitud de una cadena"
simple_title:         "Encontrando la longitud de una cadena"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# ¿Qué y por qué?

En el mundo de la programación, a menudo necesitamos saber la longitud de una cadena de texto. Esto puede ser útil para validar la entrada del usuario, manipular cadenas de texto o simplemente para obtener información sobre ellas. En Bash, encontrar la longitud de una cadena es una tarea común entre los programadores.

# Cómo hacerlo:

Para encontrar la longitud de una cadena en Bash, podemos usar el comando `echo` seguido de la opción `-n` y la cadena entre comillas. Esto imprimirá la longitud de la cadena en la salida estándar.

``` Bash
echo -n "Hola mundo" # Salida: 10
```

También podemos usar la variable de entorno `#` y la cadena dentro de una llave para obtener su longitud.

``` Bash
cadena="¡Hola!"
echo ${#cadena} # Salida: 6
```

# En profundidad:

El comando `echo` en Bash es utilizado para imprimir una o más cadenas de texto en la salida estándar. La opción `-n` desactiva el salto de línea al final de la cadena, lo que nos permite imprimir la longitud sin ningún carácter adicional. La variable de entorno `#` se expande a la longitud de la cadena que le sigue, lo que nos permite obtener su longitud directamente en lugar de usar un comando específico.

Existen otras formas de encontrar la longitud de una cadena en Bash, como usar el bucle `for` para recorrer cada carácter y mantener un contador, o usar la utilidad `wc -c` que cuenta los caracteres en un archivo. Sin embargo, las opciones mencionadas anteriormente son las más sencillas y comúnmente utilizadas.

## Ver también:

- Manual de Bash: https://www.gnu.org/software/bash/manual/bash.html
- Tutorial de Bash: https://linuxconfig.org/bash-scripting-tutorial
- Ejemplos de Bash: https://linuxconfig.org/bash-scripting-tutorial-for-beginners