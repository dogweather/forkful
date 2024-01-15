---
title:                "Leyendo argumentos de línea de comandos"
html_title:           "Bash: Leyendo argumentos de línea de comandos"
simple_title:         "Leyendo argumentos de línea de comandos"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por qué

Si estás aprendiendo Bash o simplemente quieres mejorar tus habilidades en línea de comandos, es importante entender cómo leer argumentos de línea de comandos. Esto te permitirá crear scripts más dinámicos y automatizar tareas de manera más eficiente.

## Cómo hacerlo

Para leer los argumentos de línea de comandos en Bash, puedes usar la variable especial $@. Esta variable contiene todos los argumentos pasados ​​al script en una sola línea. Veamos un ejemplo:

```Bash
#!/bin/bash
echo "Los argumentos de línea de comandos son: $@"
```

La salida de este script sería:

```Bash
$./ejemplo.sh arg1 arg2 arg3
Los argumentos de línea de comandos son: arg1 arg2 arg3
```

Como puedes ver, los argumentos se almacenan como una sola cadena, separada por espacios.

Pero ¿qué pasa si quieres leer los argumentos uno a uno? Para eso, puedes utilizar un bucle for para iterar sobre cada argumento. Veamos otro ejemplo:

```Bash
#!/bin/bash
for arg in "$@"
do
  echo "El argumento es: $arg"
done
```

La salida sería:

```Bash
$./ejemplo.sh arg1 arg2 arg3
El argumento es: arg1
El argumento es: arg2
El argumento es: arg3
```

También puedes utilizar la variable especial $#, que contiene el número total de argumentos pasados al script. Esto puede ser útil si necesitas realizar una acción específica dependiendo de la cantidad de argumentos recibidos.

## Profundizando

Existen varias opciones avanzadas para leer argumentos de línea de comandos en Bash, como flags (argumentos con valores específicos) y opciones. Puedes leer más sobre estas técnicas en la documentación oficial de Bash.

También es importante mencionar que existen herramientas de línea de comandos en Bash, como getopt, que facilitan la lectura de argumentos más complejos.

## Ver también

- [Documentación oficial de Bash](https://www.gnu.org/software/bash/)
- [Uso de getopt para leer argumentos en Bash](https://www.linuxjournal.com/content/bash-getopt-big-dogs)
- [Ejemplos avanzados de uso de argumentos en Bash](https://catonmat.net/bash-one-liners-explained-part-three)

¡Sigue practicando y pronto serás un experto en leer argumentos de línea de comandos en Bash!