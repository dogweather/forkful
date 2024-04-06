---
date: 2024-01-20 17:53:37.094152-07:00
description: "How to: (C\xF3mo hacerlo:) Hist\xF3ricamente, `console.log` ha sido\
  \ la herramienta de facto para la depuraci\xF3n r\xE1pida, un eco de los d\xEDas\
  \ cuando la impresi\xF3n en\u2026"
lastmod: '2024-04-05T22:51:12.573592-06:00'
model: gpt-4-1106-preview
summary: "(C\xF3mo hacerlo:) Hist\xF3ricamente, `console.log` ha sido la herramienta\
  \ de facto para la depuraci\xF3n r\xE1pida, un eco de los d\xEDas cuando la impresi\xF3\
  n en papel era com\xFAn."
title: "Imprimiendo salida de depuraci\xF3n"
weight: 33
---

## How to: (Cómo hacerlo:)
```TypeScript
function sumar(a: number, b: number): number {
  console.log(`Sumando ${a} + ${b}`);
  return a + b;
}

const resultado = sumar(5, 7);
console.log(`Resultado: ${resultado}`);
```
Salida:
```
Sumando 5 + 7
Resultado: 12
```

En el ejemplo, `console.log` es tu amigo para la depuración: muestra lo que necesitas saber sobre las variables y los flujos del código.

## Deep Dive (Inmersión Profunda)
Históricamente, `console.log` ha sido la herramienta de facto para la depuración rápida, un eco de los días cuando la impresión en papel era común. Hoy tenemos alternativas más sofisticadas como depuradores integrados en IDEs y herramientas especializadas como `debug` o `tracer`. Sin embargo, imprimir mensajes simples sigue siendo útil por su simplicidad y universalidad.

En TypeScript, `console.log` es solo una parte de un conjunto más amplio de métodos disponibles para la consola. Estos incluyen `console.info`, `console.error` y `console.warn`, cada uno con un propósito especial y una forma de destacar diferentes niveles de importancia o tipos de mensajes.

La implementación real detrás de estos métodos depende del entorno de ejecución (navegador o Node.js). Sin embargo, la belleza está en su consistencia: sin importar el ambiente, sabes que puedes contar con `console.log` para ver lo que sucede dentro de tu código.

## See Also (Consulta También)
- [Documentación de Console en MDN](https://developer.mozilla.org/en-US/docs/Web/API/Console)
- [TypeScript Handbook - Everyday Types](https://www.typescriptlang.org/docs/handbook/2/everyday-types.html)
- [Node.js Debugging Guide](https://nodejs.org/en/docs/guides/debugging-getting-started/)
- [Visual Studio Code Debugger](https://code.visualstudio.com/docs/editor/debugging)
