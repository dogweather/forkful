---
title:                "Manejo de errores"
date:                  2024-01-26T00:54:55.906729-07:00
model:                 gpt-4-1106-preview
simple_title:         "Manejo de errores"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/handling-errors.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

El manejo de errores es cómo gestionas cuando las cosas no van bien en tu código. Es clave porque ayuda a que tus programas fallen de manera controlada e instruye a los usuarios claramente, en lugar de simplemente colapsar y quemarse.

## Cómo hacerlo:

Aquí está el clásico bloque `try-catch`:

```javascript
try {
  // Código que podría lanzar un error
  let result = operacionPotencialmenteRiesgosa();
  console.log('Éxito:', result);
} catch (error) {
  // Qué hacer si se lanza un error
  console.error('Oops:', error.message);
}
```

Salida de muestra cuando no ocurre error:
```
Éxito: 42
```

Y cuando hay un error:
```
Oops: Algo salió mal
```

Para código asíncrono, donde se involucran promesas, utiliza `try-catch` en una función `async`:

```javascript
async function fetchData() {
  try {
    let data = await fetch('https://api.ejemplo.com/data');
    console.log('Datos obtenidos:', data);
  } catch (error) {
    console.error('Error al obtener datos:', error.message);
  }
}

fetchData();
```

## Inmersión Profunda

El manejo de errores en JavaScript ha evolucionado. En los viejos tiempos (ES3, circa 1999), solo teníamos el bloque `try-catch`. No era súper flexible, pero hacía el trabajo.

ES6 (2015) introdujo Promesas y nos dio `.then()` y `.catch()`, permitiéndonos manejar errores asíncronos de una manera más elegante.

```javascript
fetch('https://api.ejemplo.com/data')
  .then(data => console.log('Datos obtenidos:', data))
  .catch(error => console.error('Error al obtener datos:', error.message));
```

En cuanto a los detalles de implementación, cuando se lanza un error, los motores de JavaScript crean un objeto `Error` con propiedades útiles como `message` y `stack`. También puedes hacer tipos de errores personalizados extendiendo la clase `Error` – útil para aplicaciones más complejas.

¿Alternativas? Podrías ignorar el manejo de errores (mala idea), usar callbacks con parámetros que primero indican el error (hola, estilo Node.js), o ponerte más sofisticado con bibliotecas y frameworks que ofrecen sus propias soluciones.

## Ver También

Para más sobre manejo de errores:

- MDN sobre try-catch: [MDN try...catch](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Statements/try...catch)
- Async/Await: [MDN función async](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Statements/async_function)
- Una guía para Promises: [MDN Promises](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/Promise)
- Crear y lanzar errores personalizados: [MDN Error](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/Error)
