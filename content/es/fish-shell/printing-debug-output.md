---
title:                "Imprimiendo salida de depuración"
html_title:           "Arduino: Imprimiendo salida de depuración"
simple_title:         "Imprimiendo salida de depuración"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/fish-shell/printing-debug-output.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?
La impresión de la salida de depuración es una técnica que permite a los programadores ver y entender qué está pasando en su código durante la ejecución. Es esencial para identificar y corregir errores.

## Cómo hacerlo:
En Fish Shell, puedes utilizar el comando `echo` para enviar un mensaje a la salida estándar. Aquí te muestro cómo:
```Fish Shell
function hello_world
    echo "Hola Mundo"
end
hello_world
```
Y obtendrás:
```Fish Shell
Hola Mundo
```
Para el caso de depuración usa `echo`  de la siguiente forma:
```Fish Shell
function debug_output
    echo "Debug: El valor de x es $x" >&2
end

set -l x 5
debug_output
```
Y este será el resultado:
```Fish Shell
Debug: El valor de x es 5
```

## Un vistazo más a fondo
La salida de depuración es un concepto antiguo en programación y se encuentra presente en numerosos lenguajes. Su implementación en Fish Shell es bastante directa, pero hay alternativas. Una opción es usar `printf` en lugar de `echo`. 

No obstante, la opción de redireccionar la salida estándar a la salida de error con `>&2` es algo que vale la pena destacar. Esto permite mantener separados los mensajes de depuración del resultado final del programa. Si no se necesitan los mensajes de depuración, se pueden silenciar fácilmente.

## Ver también
Para profundizar en el tema, he aquí algunos recursos útiles:
1. Documentación oficial de Fish Shell: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
2. Tutorial completo sobre la salida de depuración en Fish Shell: [https://fishshell.com/tutorial.html](https://fishshell.com/tutorial.html)