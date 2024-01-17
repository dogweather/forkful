---
title:                "Interpolando una cadena"
html_title:           "Bash: Interpolando una cadena"
simple_title:         "Interpolando una cadena"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/interpolating-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Interpolar una cadena en Bash es cuando insertas variables o comandos en una cadena para mejorar su contenido dinámicamente. Los programadores lo hacen para ahorrar tiempo y agregar flexibilidad a sus scripts.

## ¿Cómo hacerlo?

Para interpolar una cadena, simplemente necesitas usar la sintaxis ```$variable``` para insertar una variable en una cadena. También puedes usar la sintaxis ```$(comando)``` para insertar la salida de un comando en una cadena.

Por ejemplo, si tienes una variable ```nombre="Juan"``` y quieres imprimir un mensaje que diga "Hola Juan, ¿cómo estás?", puedes usar la sintaxis ```echo "Hola $nombre, ¿cómo estás?"```, y el resultado será "Hola Juan, ¿cómo estás?".

## Detalles

Interpolar una cadena en Bash puede ahorrarte tiempo y mejorar la legibilidad de tu código. También es una forma conveniente de insertar variables o comandos en tus scripts. Una alternativa a la interpolación de cadenas en Bash es el uso de la función ```printf```, que te permite formatear tu salida de manera más precisa.

La interpolación de cadenas en Bash se basa en la expansión de parámetros, que es cuando el shell reemplaza un parámetro con su valor antes de ejecutar un comando o un script. También es importante tener en cuenta que la sintaxis de la interpolación de cadenas puede variar ligeramente en diferentes versiones de Bash.

## Ver también

- [Guía de Bash de Linux](https://www.tutorialspoint.com/unix_commands/bash.htm)
- [Documentación de la función printf de Bash](https://www.gnu.org/software/bash/manual/html_node/Bash-printf-Builtins.html)
- [Página de manual sobre la expansión de parámetros en Bash](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html)