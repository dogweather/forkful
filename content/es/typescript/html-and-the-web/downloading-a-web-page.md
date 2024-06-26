---
date: 2024-01-20 17:44:55.669420-07:00
description: "C\xF3mo Hacerlo: Aqu\xED te dejo un ejemplo cortito y al grano usando\
  \ `node-fetch`, una librer\xEDa muy usada para hacer peticiones HTTP que imita la\
  \ API `fetch`\u2026"
lastmod: '2024-03-13T22:44:58.799247-06:00'
model: gpt-4-1106-preview
summary: "Aqu\xED te dejo un ejemplo cortito y al grano usando `node-fetch`, una librer\xED\
  a muy usada para hacer peticiones HTTP que imita la API `fetch` del navegador."
title: "Descargando una p\xE1gina web"
weight: 42
---

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
