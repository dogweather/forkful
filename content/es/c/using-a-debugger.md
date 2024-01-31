---
title:                "Usando un depurador"
date:                  2024-01-26T03:48:02.443519-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usando un depurador"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/using-a-debugger.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Un depurador es una herramienta que te permite inspeccionar tu código C mientras se ejecuta, paso a paso, para rastrear errores. Los programadores utilizan depuradores para entender cómo se comporta su código, corregir problemas y optimizar el rendimiento sin jugar a adivinar.

## Cómo hacerlo:
Supongamos que estás trabajando con un programa simple en C que calcula el factorial de un número, pero hay un fallo. Para usar un depurador como `gdb` (GNU Debugger), primero compila con la bandera `-g` para incluir información de depuración:

```c
// compilar con: gcc factorial.c -o factorial -g
#include <stdio.h>

long factorial(int n) {
    if (n < 0) return 0; // Una simple verificación para entrada negativa
    long resultado = 1;
    while (n > 1)
        resultado *= n--;
    return resultado;
}

int main() {
    int numero = 5;
    long resultado = factorial(numero);
    printf("El factorial de %d es %ld\n", numero, resultado);
    return 0;
}
```

Luego ejecútalo en gdb:

```shell
$ gdb ./factorial
```

Establece un punto de interrupción en la función `factorial` y ejecuta el programa:

```gdb
(gdb) break factorial
(gdb) run
```

Cuando llegue al punto de interrupción, avanza a través de cada línea usando `next` o `n` e inspecciona las variables con `print` o `p`:

```gdb
(gdb) next
(gdb) print resultado
$1 = 1
```

La salida de muestra proporcionará valores en tiempo real y flujo de ejecución del programa.

## Inmersión Profunda
Los depuradores existen desde la década de 1960, evolucionando desde monitores simples hasta aplicaciones complejas basadas en GUI. La depuración basada en impresiones era común antes de que se desarrollaran depuradores maduros. Alternativas a `gdb` incluyen `lldb`, `dbx`, o depuradores integrados en IDE como los de Visual Studio o CLion.

Al tratar con depuradores, la implementación varía: algunos pueden detectar errores en tiempo de ejecución, examinar la memoria, o incluso revertir la ejecución de un programa. `gdb` puede adjuntarse a procesos en ejecución, lo que permite depurar software ya en funcionamiento, una ventaja para corregir errores en sistemas en vivo.

## Véase También
- GNU Debugger (GDB): https://www.gnu.org/software/gdb/documentation/
- Depuración con GDB: https://sourceware.org/gdb/current/onlinedocs/gdb
- Depurador LLDB: https://lldb.llvm.org/use/tutorial.html
- Técnicas de Depuración en C: http://www.cprogramming.com/debugging/debugging.html
