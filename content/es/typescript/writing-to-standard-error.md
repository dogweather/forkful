---
title:                "Escribiendo en el error estándar"
html_title:           "TypeScript: Escribiendo en el error estándar"
simple_title:         "Escribiendo en el error estándar"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

##¿Por qué escribir al error estándar en TypeScript?

Escribir al error estándar en TypeScript puede ser una herramienta útil para detectar y depurar errores en tu código. Esto te permite ver los errores en tiempo real, en lugar de tener que depender únicamente de mensajes de error en la consola del navegador.

## Cómo hacerlo

Para escribir al error estándar en TypeScript, utiliza el objeto `console` y su función `error`. Por ejemplo:

```TypeScript
console.error("Este es un error en TypeScript.");
```

Esto imprimirá el mensaje de error en la consola del navegador, permitiéndote identificar fácilmente el problema en tu código.

## Profundizando

Cuando escribas al error estándar en TypeScript, ten en cuenta que solo deberías usarlo para mensajes de error significativos. No lo uses para imprimir mensajes de depuración o para mostrar información no esencial. También puedes aprovechar otras funciones de `console` como `warn`, `info` y `log` para imprimir diferentes tipos de mensajes.

##Ver también
- [Documentación de console de TypeScript] (https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-3.html#improved-inference-for-dynamic-imports-in-umd-global-modules)
- [Uso de console en TypeScript] (https://stackify.com/node-js-console-logging/)