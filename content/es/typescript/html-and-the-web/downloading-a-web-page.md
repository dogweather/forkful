---
aliases:
- /es/typescript/downloading-a-web-page/
date: 2024-01-20 17:44:55.669420-07:00
description: "Descargar una p\xE1gina web significa traer el contenido de esa p\xE1\
  gina a tu dispositivo local o servidor. Los programadores lo hacen para an\xE1lisis\
  \ de datos,\u2026"
lastmod: 2024-02-18 23:09:09.701493
model: gpt-4-1106-preview
summary: "Descargar una p\xE1gina web significa traer el contenido de esa p\xE1gina\
  \ a tu dispositivo local o servidor. Los programadores lo hacen para an\xE1lisis\
  \ de datos,\u2026"
title: "Descargando una p\xE1gina web"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Descargar una página web significa traer el contenido de esa página a tu dispositivo local o servidor. Los programadores lo hacen para análisis de datos, monitoreo de disponibilidad de sitios web o copias de seguridad.

## Cómo Hacerlo:
Aquí te dejo un ejemplo cortito y al grano usando `node-fetch`, una librería muy usada para hacer peticiones HTTP que imita la API `fetch` del navegador.

Primero, instala `node-fetch` con npm:

```bash
npm install node-fetch
```

Luego, escribe el siguiente código TypeScript para descargar el contenido de una página web:

```typescript
import fetch from 'node-fetch';

async function descargarPagina(url: string): Promise<string> {
    const respuesta = await fetch(url);
    if (!respuesta.ok) {
        throw new Error(`Error al descargar la página: ${respuesta.statusText}`);
    }
    return await respuesta.text();
}

const url = 'http://example.com';

descargarPagina(url)
    .then(contenido => console.log(contenido))
    .catch(error => console.error(error));
```

Ejecuta tu script de TypeScript y deberías ver el contenido HTML de `http://example.com` en tu consola.

## Profundizando:
`Node-fetch` es sólo una de las muchas opciones para descargar páginas web en Node.js. Históricamente, solíamos usar el módulo `http`, que es más verboso y menos intuitivo. Otras alternativas modernas incluyen `axios` y `got`.

La implementación de `node-fetch` es muy útil, ya que puedes usar la misma lógica de la API `fetch` del navegador, lo cual es ideal para aquellos familiarizados con el front-end. Sin embargo, para scraping o descargas más complejas necesitarás algo más robusto como `cheerio` o `puppeteer`, que pueden manejar JavaScript del lado del cliente y interactuar con el DOM.

## Ver También:
Para aprender más sobre las diferentes herramientas y técnicas:

- Documentación de `node-fetch`: [https://github.com/node-fetch/node-fetch](https://github.com/node-fetch/node-fetch)
- `axios`: [https://github.com/axios/axios](https://github.com/axios/axios)
- `got`: [https://github.com/sindresorhus/got](https://github.com/sindresorhus/got)
- `cheerio` para análisis de HTML: [https://cheerio.js.org/](https://cheerio.js.org/)
- `puppeteer` para interactuar con navegadores: [https://pptr.dev/](https://pptr.dev/)
