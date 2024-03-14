---
date: 2024-01-26 00:58:10.232860-07:00
description: "El manejo de errores se refiere a esperar lo inesperado; es c\xF3mo\
  \ administramos cuando las cosas van mal en nuestro c\xF3digo. Lo hacemos para evitar\
  \ bloqueos\u2026"
lastmod: '2024-03-13T22:44:58.809189-06:00'
model: gpt-4-1106-preview
summary: "El manejo de errores se refiere a esperar lo inesperado; es c\xF3mo administramos\
  \ cuando las cosas van mal en nuestro c\xF3digo. Lo hacemos para evitar bloqueos\u2026"
title: Manejo de errores
---

{{< edit_this_page >}}

## Qué & Por qué?
El manejo de errores se refiere a esperar lo inesperado; es cómo administramos cuando las cosas van mal en nuestro código. Lo hacemos para evitar bloqueos y para brindar a los usuarios una experiencia fluida, incluso cuando ocurre lo inesperado.

## Cómo hacerlo:
En TypeScript, el manejo de errores a menudo implica bloques `try`, `catch` y `finally`.

```typescript
function riskyOperation() {
  throw new Error("¡Algo salió mal!");
}

function handleErrors() {
  try {
    riskyOperation();
  } catch (error) {
    console.error("Error capturado:", error.message);
  } finally {
    console.log("Esto siempre se ejecuta, haya error o no.");
  }
}

handleErrors();
```

Salida de muestra:

```
Error capturado: ¡Algo salió mal!
Esto siempre se ejecuta, haya error o no.
```

Ejemplo asíncrono con promesas:

```typescript
async function asyncRiskyOperation() {
  return new Promise((resolve, reject) => {
    // Simular un error
    reject("Fracaso miserablemente");
  });
}

async function handleAsyncErrors() {
  try {
    await asyncRiskyOperation();
  } catch (error) {
    console.error("Error asíncrono capturado:", error);
  }
}

handleAsyncErrors();
```

Salida de muestra:

```
Error asíncrono capturado: Fracaso miserablemente
```

## Profundización
El manejo de errores ha sido un pilar de la programación desde su aparición. En TypeScript, que se basa en JavaScript, el manejo de errores se volvió más robusto con la introducción de async/await en ECMAScript 2017. Antes de eso, a menudo nos basábamos en funciones de callback y promesas para manejar errores en el código asíncrono.

Una alternativa a `try/catch` en TypeScript es el uso de límites de error proporcionados por frameworks como React. Para el manejo del lado del servidor, podemos usar middleware en plataformas como Express.js para centralizar la gestión de errores.

En términos de implementación, TypeScript no tiene su propio mecanismo de manejo de errores sino que depende del de JavaScript. Las clases de errores personalizadas pueden extender la clase `Error` para ofrecer información de error más descriptiva.

## Ver También
- [MDN sobre try/catch](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/try...catch)
- [Async/Await en MDN](https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Asynchronous/Async_await)
- [Uso de límites de error en React](https://reactjs.org/docs/error-boundaries.html)
- [Manejo de errores en Express.js](https://expressjs.com/en/guide/error-handling.html)
