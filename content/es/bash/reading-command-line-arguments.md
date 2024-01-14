---
title:                "Bash: Lectura de argumentos de línea de comandos"
simple_title:         "Lectura de argumentos de línea de comandos"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por qué
Si estás aprendiendo a programar en Bash o estás interesado en mejorar tus habilidades en la línea de comandos, es importante que sepas cómo leer los argumentos de línea de comando. Esto te permitirá sacar el máximo provecho de tus scripts y programas escritos en Bash.

## Cómo hacerlo
Para leer los argumentos de línea de comando en Bash, primero debes entender que estos argumentos son los valores que el usuario proporciona al ejecutar un programa. Son una forma de comunicarse con el programa y cambiar su comportamiento. Para leer estos argumentos, se utiliza la variable especial "$@", que contiene una lista de todos los argumentos proporcionados por el usuario.

Veamos un ejemplo de cómo leer y usar los argumentos en Bash:

```Bash
#!/bin/bash
# Este programa recibe un argumento y lo imprime en pantalla

echo "El argumento proporcionado es: $1"
```

Al ejecutar este programa con el argumento "hola", el resultado en pantalla sería:

```
El argumento proporcionado es: hola
```

También es posible leer y utilizar varios argumentos en un mismo programa. En este caso, la variable "$@" se convierte en un arreglo, donde cada elemento contiene un argumento proporcionado por el usuario.

## Profundizando
Además de la variable "$@", existen otras formas de leer argumentos de línea de comando en Bash. Por ejemplo, también puedes utilizar la variable posicional "$1", que contiene el primer argumento, "$2" para el segundo argumento, y así sucesivamente. También puedes utilizar un bucle "for" para recorrer y utilizar todos los argumentos proporcionados por el usuario.

Es importante tener en cuenta que los argumentos de línea de comando pueden ser muy útiles, pero también deben ser utilizados con precaución. Debes asegurarte de validar y filtrar los argumentos para evitar posibles vulnerabilidades en tu programa.

## Ver también
Si quieres profundizar más en el tema de los argumentos de línea de comando en Bash, te recomendamos revisar los siguientes recursos:

- [Tutorial de argumentos de línea de comando en Bash](https://www.tutorialspoint.com/unix/commands/bash/getopt.htm)
- [Documentación oficial de Bash](https://www.gnu.org/software/bash/manual/html_node/Bash-Variables.html)
- [Otro tutorial en español](https://slimbooks.net/recursos/aprende-linux/el-arte-de-scripting-en-bash/5-argumentos-de-un-script-bash-cmd.html)

¡Esperamos que este artículo te haya ayudado a entender mejor cómo leer los argumentos de línea de comando en Bash! Ahora puedes utilizar esta habilidad en tus próximos proyectos y scripts.