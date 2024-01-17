---
title:                "Enviando una solicitud http"
html_title:           "Javascript: Enviando una solicitud http"
simple_title:         "Enviando una solicitud http"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?
Enviar una solicitud HTTP es una forma en que los programadores pueden comunicarse con servidores web. Esto les permite obtener información o realizar acciones en el servidor. Los programadores a menudo usan solicitudes HTTP para crear sitios web interactivos o aplicaciones basadas en la web.

## Cómo hacerlo:
```javascript
const request = new XMLHttpRequest();
request.onreadystatechange = function() {
  if (this.readyState == 4 && this.status == 200) {
    console.log(this.responseText);
  }
};
request.open("GET", "https://example.com");
request.send();
```

Este ejemplo usa la función `XMLHttpRequest` para crear una nueva solicitud. Luego, se establece una función de devolución de llamada para manejar la respuesta del servidor. Finalmente, se especifica el tipo de solicitud y la URL del servidor, y se envía la solicitud.

El resultado devuelto por el servidor se puede ver en la consola utilizando `console.log(this.responseText)`.

## Inmersión Profunda:
La función `XMLHttpRequest` fue introducida en 1999 por Microsoft para permitir que Internet Explorer se comuniquen con servidores web. Sin embargo, en 2006, se estandarizó como parte de la especificación del objeto `XMLHttpRequest`.

Hay otras formas de realizar solicitudes HTTP, como utilizando la API `fetch` o bibliotecas de terceros como Axios o jQuery. Sin embargo, `XMLHttpRequest` sigue siendo ampliamente utilizado debido a su compatibilidad con navegadores antiguos.

## Ver También:
- [Documentación de MDN sobre XMLHttpRequest](https://developer.mozilla.org/es/docs/Web/API/XMLHttpRequest)
- [Especificación de XMLHttpRequest](https://developer.mozilla.org/es/docs/Web/API/XMLHttpRequest/XMLHttpRequest)
- [Documentación de MDN sobre la API Fetch](https://developer.mozilla.org/es/docs/Web/API/Fetch_API)