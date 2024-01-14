---
title:    "Fish Shell: Leyendo argumentos de línea de comando"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## ¿Por qué leer argumentos de línea de comandos?

La lectura de argumentos de línea de comandos es una habilidad esencial para cualquier programador. Al aprender a leer y manipular los argumentos proporcionados por el usuario en la terminal, puedes crear scripts y programas más interactivos y dinámicos.

## Cómo hacerlo

Fish Shell tiene una función integrada llamada `argv` que devuelve una lista de todos los argumentos proporcionados en la línea de comandos. Puedes acceder a cada argumento individualmente utilizando un índice numérico.

```
Fish Shell Code:

#!/usr/bin/fish

# Ejemplo de lectura de argumentos de línea de comandos
echo "¡Hola, $argv[1]! ¿Cómo estás?"

```

```
Output:

$ ./ejemplo.fish John
¡Hola, John! ¿Cómo estás?
```

## Profundizando

Existen varias técnicas avanzadas que puedes utilizar al leer los argumentos de línea de comandos en Fish Shell. Por ejemplo, puedes utilizar la bandera `-s` en la función `argv` para obtener solo los campos después de una cierta posición. También puedes utilizar la función `status` para verificar si se proporcionaron suficientes argumentos en la línea de comandos y manejar errores o problemas.

## Ver también

- La documentación oficial de Fish Shell sobre la función `argv`: https://fishshell.com/docs/current/cmds/argv.html
- Un tutorial detallado sobre cómo leer argumentos de línea de comandos en Fish Shell: https://dev.to/vrandkode/reading-command-line-arguments-in-fish-shell-47l6
- Otros tipos de manipulación de argumentos de línea de comandos en Fish Shell: https://stackoverflow.com/questions/38169048/how-to-read-the-number-of-command-line-arguments-in-fish-shell