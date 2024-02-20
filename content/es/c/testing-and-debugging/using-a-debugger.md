---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:09:48.209956-07:00
description: "Los depuradores en C son herramientas especializadas que permiten a\
  \ los desarrolladores avanzar paso a paso a trav\xE9s de su c\xF3digo, inspeccionar\
  \ variables\u2026"
lastmod: 2024-02-19 22:05:18.057827
model: gpt-4-0125-preview
summary: "Los depuradores en C son herramientas especializadas que permiten a los\
  \ desarrolladores avanzar paso a paso a trav\xE9s de su c\xF3digo, inspeccionar\
  \ variables\u2026"
title: Utilizando un depurador
---

{{< edit_this_page >}}

## Qué y Por Qué?

Los depuradores en C son herramientas especializadas que permiten a los desarrolladores avanzar paso a paso a través de su código, inspeccionar variables y monitorear el flujo de ejecución. Este proceso es fundamental para identificar y corregir errores, asegurando que el código se comporte como se espera.

## Cómo hacerlo:

El depurador de GNU (GDB) es el depurador más utilizado para la programación en C. Aquí hay una breve guía sobre cómo usar GDB para depurar un simple programa en C.

Primero, compila tu programa en C con la bandera `-g` para incluir información de depuración:

```c
gcc -g program.c -o program
```

A continuación, inicia GDB con tu programa compilado:

```bash
gdb ./program
```

Ahora puedes usar varios comandos dentro de GDB para controlar su operación. Aquí hay algunos comandos fundamentales:

- `break`: Establece un punto de interrupción en una línea especificada o función para pausar la ejecución.
  - Ejemplo: `break 10` o `break main`
- `run`: Inicia la ejecución de tu programa dentro de GDB.
- `next`: Ejecuta la próxima línea de código sin entrar en funciones.
- `step`: Ejecuta la próxima línea de código, entrando en funciones.
- `print`: Muestra el valor de una variable.
- `continue`: Reanuda la ejecución hasta el próximo punto de interrupción.
- `quit`: Salir de GDB.

Aquí hay una sesión de ejemplo depurando un programa simple:

```c
#include <stdio.h>

int main() {
    int i;
    for (i = 0; i < 5; i++) {
        printf("%d\n", i);
    }
    return 0;
}
```

Compila e inicia GDB como se describió. Establece un punto de interrupción en la línea de `printf` con `break 5` y luego `run`. Usa `next` para avanzar a través del bucle y `print i` para inspeccionar la variable del bucle.

Salida de muestra después de establecer un punto de interrupción y antes de la primera iteración:

```
Breakpoint 1, main () at program.c:5
5         printf("%d\n", i);
```

Usando `print i` después de algunas iteraciones:

```
$3 = 2
```

Esto demuestra el examen del estado y flujo de un programa simple.

## Inmersión Profunda

El concepto de depuración ha evolucionado significativamente desde los primeros días de la programación, donde errores físicos (insectos literales) podrían causar problemas en computadoras mecánicas. Hoy en día, depuradores como GDB ofrecen características sofisticadas más allá de los básicos de avanzar paso a paso y la inspección de variables, tales como la depuración inversa (ejecutar el programa hacia atrás), puntos de interrupción condicionales y scripting para tareas de depuración automatizadas.

Aunque GDB es poderoso y ampliamente utilizado, puede ser denso y desafiante para los principiantes. Herramientas alternativas de depuración y IDEs (Entornos de Desarrollo Integrados) como Visual Studio Code, CLion o Eclipse ofrecen interfaces más amigables para depurar código C, a menudo integrando ayudas visuales y controles más intuitivos. Estas alternativas pueden no ofrecer toda la profundidad funcional de GDB, pero pueden ser más accesibles para los recién llegados a la programación en C.

Además, la aparición de protocolos de servidores de lenguajes y estándares de depuración ha facilitado soluciones de depuración multiplataforma, haciendo la experiencia de depuración más consistente en diferentes herramientas y entornos. A pesar de estos avances, aprender los detalles de un depurador tradicional como GDB proporciona una visión invaluable sobre la ejecución de programas en C y sigue siendo una habilidad crucial en el kit de herramientas de un desarrollador.
