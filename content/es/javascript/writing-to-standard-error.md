---
title:                "Javascript: Escribiendo en el error estándar"
programming_language: "Javascript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

##Por qué: Escribir a Standard Error en Javascript

Escribir a Standard Error en Javascript es una práctica común y útil para los programadores. Al usar esta técnica, podrás imprimir información de depuración y errores en la consola del navegador, lo que te permitirá detectar y solucionar problemas en tu código de manera más eficiente.

##Cómo hacerlo:

Para escribir a Standard Error en Javascript, simplemente debes usar el método `console.error()` y pasar como parámetro el mensaje que deseas imprimir. Por ejemplo:

```Javascript
console.error("Ha ocurrido un error en la función");
```

Esto imprimirá el mensaje en rojo en la consola del navegador, lo que lo distinguirá de los demás mensajes de debug y facilitará su identificación.

##Profundizando:

Escribir a Standard Error no solo es útil para imprimir mensajes de error, también puede ser utilizado para imprimir información de depuración durante el desarrollo de una aplicación. Esto te permitirá rastrear el proceso del código y detectar posibles bugs o problemas en tu lógica.

Además, en lugar de solo imprimir un mensaje, también puedes pasar como parámetro objetos, arreglos o variables, lo que te permitirá visualizar de manera más detallada la información que necesitas en la consola.

Otra técnica útil es utilizar `console.trace()`, que te mostrará la pila de llamadas que ha llevado al error o mensaje de debug, lo que puede ser muy útil para rastrear su origen.

##Ver también:

- Documentación de console.error() en MDN: https://developer.mozilla.org/es/docs/Web/API/Console/error
- Tutorial en español sobre cómo usar console.error(): https://www.onlinecode.org/javascript-console-error/
- Ejemplo práctico de uso de console.error() para detectar un error en Javascript: https://codeburst.io/error-handling-in-javascript-a6172ccdf9af

¡Ahora que ya conoces cómo y por qué escribir a Standard Error en Javascript, inténtalo en tu próximo proyecto y haz tu proceso de depuración más eficiente!