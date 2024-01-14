---
title:                "TypeScript: Descargar una página web"
simple_title:         "Descargar una página web"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Por qué

Descargar una página web es una habilidad útil en el mundo del desarrollo web. Puede ser útil para realizar pruebas, realizar análisis o simplemente almacenar una copia de la página para uso futuro. En este artículo, exploraremos cómo podemos descargar una página web utilizando TypeScript.

## Cómo hacerlo

Para descargar una página web en TypeScript, primero necesitamos importar el módulo `https` de Node.js. Luego, podemos utilizar la función `get()` para realizar una solicitud HTTP GET a la URL que queremos descargar. A continuación, utilizaremos la función `pipe()` para redirigir la respuesta a un archivo en nuestro sistema.

```TypeScript
// Importamos el módulo https
import * as https from 'https';
// Importamos el módulo fs para manejo de archivos
import * as fs from 'fs';

// URL de la página que queremos descargar
const url = 'https://ejemplo.com';
// Ruta donde queremos guardar el archivo descargado
const path = './ejemplo.html';

// Realizamos la solicitud GET
https.get(url, (res) => {
    // Redirigimos la respuesta al archivo
    res.pipe(fs.createWriteStream(path));
})
```

Una vez que se completa la descarga, encontraremos el archivo `ejemplo.html` en la ruta especificada, listo para ser utilizado.

## Profundizando

Hay varias opciones que podemos considerar al descargar una página web. Por ejemplo, podemos usar una librería como `cheerio` para analizar y extraer datos específicos de la página descargada. También podemos configurar la solicitud para incluir encabezados de agente de usuario y cookies si es necesario.

Otra consideración importante es manejar posibles errores al descargar una página. Podemos utilizar el bloque `try-catch` para manejar excepciones o podemos verificar el código de estado de la respuesta para asegurarnos de que se descargó correctamente.

## Ver también

- Documentación oficial de Node.js `https` módulo: https://nodejs.org/api/https.html
- Documentación de TypeScript: https://www.typescriptlang.org/
- Librería `cheerio`: https://cheerio.js.org/