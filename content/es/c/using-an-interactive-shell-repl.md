---
title:                "Usando una shell interactiva (REPL)"
date:                  2024-01-26T04:11:24.834124-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usando una shell interactiva (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Una shell interactiva, o Bucle Leer-Evaluar-Imprimir (REPL, por sus siglas en inglés), es una herramienta que proporciona un entorno de codificación en tiempo real para probar fragmentos de código al instante. Los programadores la utilizan para obtener feedback rápido durante el desarrollo, el aprendizaje y la depuración.

## Cómo hacerlo:
C no viene con un REPL incorporado, pero puedes usar herramientas de terceros. Aquí hay un vistazo usando Cling, un intérprete de C++ que también puede manejar código C:

```C
#include <stdio.h>

int main() {
    printf("¡Hola, mundo REPL!\n");
    return 0;
}
```

Salida en Cling REPL:
```
[cling]$ .x tuscript.c
¡Hola, mundo REPL!
```

Cling ejecuta el script e imprime la salida al instante.

## Profundización
Los REPL son estándar en lenguajes dinámicos como Python o Ruby, pero para lenguajes compilados como C, son menos comunes. Históricamente, el ciclo de compilar-ejecutar-depurar no se prestaba para la exploración interactiva. Herramientas como Cling y compiladores en línea de C ofrecen experiencias similares a un REPL envolviendo tu código C en un entorno de C++.

Las alternativas a Cling incluyen intérpretes de C como CINT y Ch. Estas herramientas permiten una iteración rápida pero pueden no ser adecuadas para todos los escenarios de desarrollo debido a las limitaciones de rendimiento y el soporte para características complejas.

La implementación de un REPL en un lenguaje compilado implica compilar y ejecutar fragmentos de código sobre la marcha, lo cual no es trivial y puede tener limitaciones en comparación con las capacidades completas del lenguaje.

## Ver también
- Cling: https://github.com/root-project/cling
- Compilador y REPL de C en línea: https://repl.it/languages/c
- CINT: http://root.cern.ch/drupal/content/cint
- Intérprete Ch: http://www.softintegration.com/products/chstandard/
