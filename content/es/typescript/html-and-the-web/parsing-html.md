---
date: 2024-01-20 15:34:10.927158-07:00
description: "C\xF3mo Hacerlo: Digamos que quieres extraer todos los enlaces de una\
  \ p\xE1gina web. Usando TypeScript, puedes emplear librer\xEDas como `node-html-parser`\
  \ para\u2026"
lastmod: '2024-03-13T22:44:58.798217-06:00'
model: unknown
summary: "Digamos que quieres extraer todos los enlaces de una p\xE1gina web."
title: "An\xE1lisis de HTML"
weight: 43
---

## Cómo Hacerlo:
Digamos que quieres extraer todos los enlaces de una página web. Usando TypeScript, puedes emplear librerías como `node-html-parser` para hacer el trabajo pesado.

```typescript
import { parse } from 'node-html-parser';

const html = `<html><body><a href="https://ejemplo.com">Enlace</a></body></html>`;
const root = parse(html);

const enlaces = root.querySelectorAll('a').map((element) => element.getAttribute('href'));
console.log(enlaces);
```

Y la salida será algo como esto:

```
[ 'https://ejemplo.com' ]
```

## Profundización:
Históricamente, parsear HTML era un desafío debido a la variabilidad y la complejidad del HTML en la web. Las bibliotecas modernas como `node-html-parser` y `Cheerio` han simplificado mucho este proceso. Aún así, la implementación del análisis puede variar entre las herramientas: algunas construyen un `Document Object Model (DOM)` completo, mientras que otras optan por un modelo más sencillo y rápido especialmente útil en el entorno de `Node.js`.

A diferencia de `DOMParser` en el navegador, estas librerías están diseñadas para ser usadas con `Node.js` y no dependen de un navegador para funcionar. Esto las hace ideales para el scraping web y la automatización de tareas en servidores.

## Ver También:
- Documentación de `node-html-parser`: [https://github.com/taoqf/node-html-parser](https://github.com/taoqf/node-html-parser)
- Cheerio, otra biblioteca poderosa: [https://cheerio.js.org/](https://cheerio.js.org/)
- Para entender más sobre cómo funciona el DOM: [https://developer.mozilla.org/es/docs/Web/API/Document_Object_Model](https://developer.mozilla.org/es/docs/Web/API/Document_Object_Model)
