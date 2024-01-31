---
title:                "Descargando una página web"
date:                  2024-01-20T17:44:18.283149-07:00
model:                 gpt-4-1106-preview
simple_title:         "Descargando una página web"

category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?
Descargar una página web significa obtener su HTML, datos y multimedia. Los programadores lo hacen para análisis de datos, pruebas o copias de seguridad.

## How to:
Usaremos `fetch` para descargar una página:

```javascript
async function descargarPagina(url) {
  const respuesta = await fetch(url);
  const contenido = await respuesta.text();
  console.log(contenido);
}

descargarPagina('https://example.com');
```

Muestra de salida esperada (fragmento de HTML de example.com):

```html
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
```

## Deep Dive
Antes de `fetch`, el objeto `XMLHttpRequest` reinaba, pero era engorroso. Fetch ofrece una manera más simple y limpia con promesas. Hay alternativas como Axios, pero `fetch` es suficiente y nativo. Detrás de escena, `fetch` envía un HTTP GET al servidor, que responde con los contenidos de la página.

## See Also
- Documentación de MDN sobre `fetch`: [MDN fetch](https://developer.mozilla.org/es/docs/Web/API/Fetch_API)
- Guía de Axios: [Axios on GitHub](https://github.com/axios/axios)
- Tutorial sobre `XMLHttpRequest`: [MDN XMLHttpRequest](https://developer.mozilla.org/es/docs/Web/API/XMLHttpRequest)
