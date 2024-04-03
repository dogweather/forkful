---
date: 2024-01-26 04:18:22.962589-07:00
description: "C\xF3mo hacerlo: TypeScript no viene con su propio REPL. Vamos a usar\
  \ `ts-node`, un entorno de ejecuci\xF3n de TypeScript para Node.js que incluye un\
  \ REPL.\u2026"
lastmod: '2024-03-13T22:44:58.802265-06:00'
model: gpt-4-0125-preview
summary: TypeScript no viene con su propio REPL.
title: Usando una shell interactiva (REPL)
weight: 34
---

## Cómo hacerlo:
TypeScript no viene con su propio REPL. Vamos a usar `ts-node`, un entorno de ejecución de TypeScript para Node.js que incluye un REPL.

Primero, instálalo globalmente:
```bash
npm install -g ts-node
```

Inicia el REPL escribiendo `ts-node` en tu línea de comandos:
```bash
ts-node
```

Aquí tienes un fragmento rápido para probar:
```TypeScript
> let message: string = '¡Hola, REPL!';
> console.log(message);
¡Hola, REPL!
> 
```
Para terminar la sesión, presiona `Ctrl+D`.

## Profundización
Históricamente, los REPL eran prominentes en lenguajes como Lisp, permitiendo la evaluación dinámica de código. El concepto se ha extendido desde entonces, convirtiéndose en un elemento básico para la codificación interactiva en muchos lenguajes.

Para TypeScript, `ts-node` no es tu única opción. Las alternativas incluyen usar el Playground de TypeScript en un navegador web o aprovechar otros REPL basados en Node.js que admitan TypeScript con complementos adecuados.

En términos de implementación, `ts-node` utiliza la API del compilador de TypeScript para transpilar código al vuelo antes de que sea ejecutado por Node.js. Esto te brinda retroalimentación inmediata y es particularmente útil para probar las últimas características de TypeScript sin complicaciones de configuración.

Una cosa a recordar: aunque un REPL es excelente para pruebas rápidas, no reemplaza escribir código tradicional, testeable y mantenible. Es una herramienta para aprender y explorar, no un sustituto de prácticas de desarrollo adecuadas.

## Ver También
- [Sitio Oficial de TypeScript](https://www.typescriptlang.org/)
- [ts-node en GitHub](https://github.com/TypeStrong/ts-node)
- [Documentación de REPL de Node.js](https://nodejs.org/api/repl.html)
- [Playground de TypeScript](https://www.typescriptlang.org/play)
