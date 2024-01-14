---
title:                "TypeScript: Enviando una solicitud http"
simple_title:         "Enviando una solicitud http"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Por qué enviar una solicitud HTTP en TypeScript

Si has estado aprendiendo TypeScript, probablemente ya sepas cómo declarar variables, crear funciones y clases, y manejar errores. Pero, ¿sabías que también puedes enviar solicitudes HTTP utilizando este lenguaje de programación? En este artículo, te explicaremos por qué es importante aprender a enviar solicitudes HTTP y cómo hacerlo en TypeScript.

## Cómo hacerlo

Antes de mostrar cómo enviar una solicitud HTTP en TypeScript, es importante entender qué es una solicitud HTTP y por qué la necesitamos. Una solicitud HTTP es una comunicación entre el cliente (un navegador o una aplicación) y un servidor. Puedes enviar una solicitud HTTP para obtener información de una página web o para enviar datos al servidor, por ejemplo, al completar un formulario.

Para enviar una solicitud HTTP en TypeScript, primero debes importar el módulo `http` de Node.js. Luego, puedes utilizar la función `request` para crear una solicitud. Aquí hay un ejemplo de cómo se vería esto en código:

```TypeScript
import * as http from 'http';

// Creamos la solicitud y pasamos la URL del servidor
const request = http.request('http://localhost:3000', (response) => {
  // Manejamos la respuesta del servidor
  console.log(response.statusCode); // Muestra el código de estado de la respuesta
});

// Enviamos la solicitud al servidor
request.end();
```

Como puedes ver, primero importamos el módulo `http` y luego llamamos a la función `request` con la URL del servidor como parámetro. Luego, creamos una función de respuesta que manejará la respuesta del servidor y finalmente enviamos la solicitud con la función `end()`.

## Profundizando

Ahora que sabes cómo enviar una solicitud HTTP en TypeScript, es importante profundizar un poco más y entender mejor cómo funciona este proceso. Una solicitud HTTP consta de un método (como GET, POST, PUT o DELETE), una ruta y opcionalmente, datos que se pueden enviar al servidor. Además de eso, también puede haber encabezados que proporcionan información adicional.

En el ejemplo anterior, no especificamos el método ya que por defecto se utiliza GET. Sin embargo, puedes especificar un método diferente utilizando la opción `method` al llamar a la función `request`. También puedes enviar datos al servidor utilizando la opción `data`, y especificar encabezados con la opción `headers`.

## Ver también

- [HTTP en TypeScript - Documentación oficial](https://www.typescriptlang.org/docs/handbook/integrating-with-build-tools.html)
- [Módulo HTTP de Node.js - Documentación oficial](https://nodejs.org/api/http.html)
- [Haciendo solicitudes HTTP en TypeScript - Medium](https://medium.com/better-programming/doing-http-requests-in-typescript-3efbba07c4b8)