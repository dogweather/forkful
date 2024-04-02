---
date: 2024-01-26 03:50:04.836379-07:00
description: "Aqu\xED tienes un fragmento de c\xF3digo en JavaScript que no se est\xE1\
  \ comportando como se esperaba: ```javascript function buggyMultiply(a, b) { return\
  \ a + b;\u2026"
lastmod: '2024-03-13T22:44:59.463271-06:00'
model: gpt-4-0125-preview
summary: "Aqu\xED tienes un fragmento de c\xF3digo en JavaScript que no se est\xE1\
  \ comportando como se esperaba: ```javascript function buggyMultiply(a, b) { return\
  \ a + b;\u2026"
title: Usando un depurador
weight: 35
---

## Cómo hacerlo:
Aquí tienes un fragmento de código en JavaScript que no se está comportando como se esperaba:

```javascript
function buggyMultiply(a, b) {
    return a + b; // ¡Ups! Esto debería ser una multiplicación, no una adición.
}

let result = buggyMultiply(5, 3);
console.log('Resultado:', result);
```

La salida es incorrecta:
```
Resultado: 8
```

Vamos a depurar en Chrome DevTools:

1. Abre este JS en un navegador.
2. Haz clic derecho y selecciona "Inspeccionar" para abrir DevTools.
3. Haz clic en la pestaña "Sources".
4. Encuentra tu fragmento de código o página y coloca un punto de interrupción haciendo clic en el número de línea al lado de la declaración `return`.
5. Actualiza la página para activar el punto de interrupción.
6. Revisa el panel "Scope" para ver las variables locales `a` y `b`.
7. Avanza con el botón "Pasar sobre la siguiente llamada de función".
8. Identifica el error en la declaración `return`.
9. Arregla el código:
```javascript
function buggyMultiply(a, b) {
    return a * b; // ¡Arreglado!
}

let result = buggyMultiply(5, 3);
console.log('Resultado:', result);
```

La salida corregida:
```
Resultado: 15
```

## Inmersión Profunda
El concepto de depuración existe desde los primeros días de la informática; ¡la leyenda dice que comenzó cuando se encontró una polilla en un ordenador en la década de 1940! Hoy en día, los depuradores de JavaScript, como las herramientas integradas en el navegador (Herramientas para Desarrolladores de Chrome, Herramientas para Desarrolladores de Firefox) o los depuradores integrados en IDE (Visual Studio Code, WebStorm) ofrecen una tonelada de características.

Alternativas a los depuradores integrados incluyen herramientas de terceros como WebStorm o usar el viejo y buen `console.log` para mostrar el estado de las variables. Pero estos no ofrecen la interacción en tiempo real y la inspección detallada proporcionada por los depuradores.

En cuanto a los detalles de implementación, la mayoría de los depuradores funcionan de manera similar: te permiten establecer puntos de interrupción que pausan la ejecución, avanzar a través del código, inspeccionar el estado actual de las variables, observar expresiones e incluso manipular valores sobre la marcha para probar diferentes escenarios.

## Ver También
- [Herramientas para Desarrolladores de Google Chrome](https://developers.google.com/web/tools/chrome-devtools)
- [Red de Desarrolladores de Mozilla - Depurador de Firefox](https://developer.mozilla.org/es/docs/Tools/Debugger)
- [Visual Studio Code - Depuración](https://code.visualstudio.com/docs/editor/debugging)
