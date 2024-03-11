---
date: 2024-01-26 04:15:21.225736-07:00
description: "Las terminales interactivas, o REPLs (Read-Eval-Print Loops, Bucles\
  \ de Leer-Evaluar-Imprimir), te permiten ejecutar c\xF3digo sobre la marcha, probando\u2026"
lastmod: '2024-03-11T00:14:33.291765-06:00'
model: gpt-4-0125-preview
summary: "Las terminales interactivas, o REPLs (Read-Eval-Print Loops, Bucles de Leer-Evaluar-Imprimir),\
  \ te permiten ejecutar c\xF3digo sobre la marcha, probando\u2026"
title: Usando una shell interactiva (REPL)
---

{{< edit_this_page >}}

## Qué & Por qué?
Las terminales interactivas, o REPLs (Read-Eval-Print Loops, Bucles de Leer-Evaluar-Imprimir), te permiten ejecutar código sobre la marcha, probando funciones, algoritmos o jugueteando con ideas. Son los blocs de notas del código, rápidos y sucios, sin necesidad de configurar un entorno de desarrollo completo.

## Cómo hacerlo:
Node.js viene con un REPL accesible a través de la terminal. Ábrelo, y estás listo para comenzar. Aquí un adelanto:

```javascript
$ node
> let sum = (a, b) => a + b;
indefinido
> sum(5, 10);
15
> .exit
```

Sencillo, ¿verdad? Define variables, funciones o ejecuta bucles. Cuando termines, `.exit` te devuelve al mundo real.

## Inmersión Profunda
Los REPLs existen desde la década de 1960 – LISP fue el pionero del concepto. La idea: proporcionar retroalimentación inmediata al programador. ¿Alternativas? Además de REPL de Node.js, hay consolas basadas en el navegador como Chrome DevTools, arenas de juego en línea como JSFiddle o IDEs completos como VSCode con espacios de juego interactivos.

Por debajo del capó, los flujos de trabajo de REPL típicamente:
1. Leen la entrada
2. Compilan y ejecutan el código
3. Imprimen la salida
4. Vuelven al principio

Es un ciclo simple pero efectivo que ha influenciado masivamente la codificación interactiva.

## Ver También
- [Documentación de REPL de Node.js](https://nodejs.org/api/repl.html)
- [Introducción a los módulos de JavaScript en REPLs de Mozilla](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Modules)
- [JSFiddle](https://jsfiddle.net/)
