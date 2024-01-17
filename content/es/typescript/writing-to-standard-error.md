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

## ¿Qué y Por qué? 
Escribir a la salida de error estándar (stderr) es una técnica común utilizada por los programadores para mostrar mensajes de error en la consola al momento de ejecutar su código. Es una manera útil de informar al desarrollador acerca de errores que pueden ocurrir durante la ejecución de un programa. 

## Cómo: 
Escribir a la salida de error estándar en TypeScript se puede lograr usando la función `console.error()`. Este método acepta uno o varios parámetros que serán mostrados como un mensaje de error en la consola al momento de ejecutar el programa. Por ejemplo: 

```TypeScript
console.error("¡Algo salió mal!");
console.error("Ocurrió un error en la línea " + lineNumber);
```

Esto mostrará un mensaje de error en la consola de la siguiente manera: 
> ¡Algo salió mal! <br>
> Ocurrió un error en la línea 10

## Profundizando: 
La técnica de escribir a stderr tiene sus raíces en los sistemas operativos Unix, donde stderr es un flujo de salida donde se imprimen los mensajes de error. Los programas suelen utilizar stdout (standard output) para el flujo de salida normal y stderr para los mensajes de error. 

Existen alternativas para mostrar mensajes de error en TypeScript, como utilizar el módulo `console` de Node.js o crear una instancia de `Error` y lanzarla con `throw`. Sin embargo, escribir a stderr sigue siendo una forma sencilla y útil de mostrar mensajes de error en la consola. 

En cuanto a la implementación, la función `console.error()` simplemente envía el mensaje de error al flujo de salida stderr del proceso actual. En los sistemas operativos Windows, esto se mostrará en la consola como si se hubiera utilizado `console.log()`.

## Para saber más:
Si quieres profundizar más en el uso de `console.error()` en TypeScript, puedes consultar la documentación oficial de la función o explorar otros métodos y propiedades disponibles en el objeto `console`. También puedes investigar más sobre los conceptos de stdout y stderr y cómo se utilizan en diferentes sistemas operativos.