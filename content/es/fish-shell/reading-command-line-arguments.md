---
title:                "Fish Shell: Leyendo argumentos de línea de comando"
simple_title:         "Leyendo argumentos de línea de comando"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por qué

Si eres un programador, probablemente estés acostumbrado a utilizar una interfaz gráfica para manejar tu código. Sin embargo, existe otra forma de interactuar con tu computadora y es a través de la línea de comandos. Aunque puede parecer intimidante al principio, aprender a utilizar la línea de comandos puede ser muy beneficioso, especialmente cuando se trata de leer y utilizar argumentos de línea de comandos.

## Cómo

Fish Shell es un tipo de terminal que permite a los usuarios interactuar con su sistema operativo a través de la línea de comandos. Una de las funciones más útiles de Fish Shell es la capacidad de leer y utilizar argumentos de línea de comandos en tus scripts o programas.

Para leer un argumento de línea de comandos en Fish Shell, se utiliza la variable especial `$argv`. Esta variable contiene todos los argumentos que se han pasado al comando, separados por espacios. Por ejemplo, si se ejecuta el script `hola.fish` de la siguiente manera:

```
fish hola.fish Juan Marta Pedro
```

La variable `$argv` contendrá los valores `Juan`, `Marta` y `Pedro`.

Para acceder a cada argumento de manera individual, se utiliza `$argv[1]` para el primer argumento, `$argv[2]` para el segundo, y así sucesivamente.

## Deep Dive

Los argumentos de línea de comandos pueden ser de gran utilidad cuando se trata de automatizar tareas u obtener información específica del usuario. Por ejemplo, puedes crear un script que te permita copiar y renombrar archivos en base a los argumentos que le pases. También puedes utilizar argumentos de línea de comandos en tus programas para personalizar su funcionamiento según la necesidad del usuario.

Es importante mencionar que Fish Shell también cuenta con la variable especial `$argc`, que contiene el número total de argumentos pasados al comando.

## See Also

Si quieres saber más sobre cómo utilizar Fish Shell y sus diferentes funcionalidades, aquí te dejamos algunos enlaces útiles:

- [Introducción a Fish Shell](https://fishshell.com/)
- [Documentación oficial de Fish Shell](https://fishshell.com/docs/current/)
- [Tutorial de línea de comandos para principiantes](https://tutorial.djangogirls.org/es/intro_to_command_line/)
- [Tutorial de línea de comandos avanzado](https://www.learnenough.com/command-line-tutorial)

¡Esperamos que este artículo te haya sido útil para aprender sobre la lectura de argumentos de línea de comandos en Fish Shell! Y recuerda, ¡practicar es la clave para dominar cualquier habilidad en programación! ¡Hasta pronto!