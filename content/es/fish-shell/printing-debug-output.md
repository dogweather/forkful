---
title:                "Imprimiendo resultados de depuración"
html_title:           "Fish Shell: Imprimiendo resultados de depuración"
simple_title:         "Imprimiendo resultados de depuración"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por qué
Si eres un programador en Fish Shell, es probable que te encuentres en situaciones en las que necesites encontrar errores o problemas en tu código. Una forma útil de hacerlo es imprimiendo la salida de depuración, lo que te permite ver el estado y los valores de las variables en diferentes puntos de tu programa. En este artículo, te enseñaremos cómo imprimir la salida de depuración en Fish Shell.

## Cómo hacerlo
Para imprimir la salida de depuración en Fish Shell, simplemente usa el comando `echo` seguido de la variable o valor que quieras imprimir. Por ejemplo, si quieres imprimir el valor de la variable `numero`, puedes hacerlo de la siguiente manera:

```Fish Shell
echo $numero
```

Esto imprimirá el valor actual de la variable `numero` en tu terminal. También puedes imprimir múltiples variables o valores en una sola línea separándolos por un espacio:

```Fish Shell
echo $variable1 $variable2 $valor
```

Además, puedes imprimir mensajes de texto junto con tus variables o valores para hacer tus mensajes de depuración más descriptivos:

```Fish Shell
echo "El valor de la variable numero es: " $numero
```

Esto imprimirá "El valor de la variable numero es: [valor de numero]" en tu terminal.

## Deep Dive
Cuando imprimes la salida de depuración en Fish Shell, es importante tener en cuenta que solo estás viendo una instantánea del estado de tu programa en ese momento específico. Si quieres ver cómo cambian los valores de tus variables a medida que avanza tu programa, puedes usar el comando `printf` en lugar de `echo`:

```Fish Shell
printf "Valor actual de numero: %s\n" $numero
```

Este comando imprimirá el valor de `numero` en el momento en que se ejecuta, seguido de un salto de línea. Esto es especialmente útil cuando estás realizando bucles o iteraciones y quieres ver cómo cambian los valores en cada iteración.

Además, también puedes usar la opción `--debug` al ejecutar tu script de Fish Shell para que te muestre automáticamente todas las salidas de depuración:

```Fish Shell
fish --debug mi_script.fish
```

Esto imprimirá todas las salidas de depuración en tu terminal mientras tu script se está ejecutando.

## Ver también
- [Documentación oficial de Fish Shell](https://fishshell.com/docs/current/)
- [Tutorial de Fish Shell](https://medium.com/@jorgebucaran/fish-shell-generation-ebb5062db77a)
- [Artículo sobre cómo utilizar Fish Shell como tu shell por defecto](https://linuxhint.com/fish_shell_mac/)