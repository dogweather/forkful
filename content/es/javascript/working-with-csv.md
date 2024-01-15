---
title:                "Trabajando con archivos csv"
html_title:           "Javascript: Trabajando con archivos csv"
simple_title:         "Trabajando con archivos csv"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/working-with-csv.md"
---

{{< edit_this_page >}}

## ¿Por qué trabajar con CSV?

CSV (Comma-Separated Values) es un formato simple y universalmente compatible para almacenar datos estructurados en forma de tabla. Trabajar con archivos CSV te permite manipular grandes cantidades de datos de forma eficiente, realizar análisis y generar informes o visualizaciones.

## Cómo hacerlo
Trabajar con archivos CSV en Javascript es sencillo gracias a la librería externa "csv-parser". Primero, debemos instalarla utilizando el manejador de paquetes npm:

```
npm install csv-parser
```

Luego, importamos la librería en nuestro archivo de Javascript:

```javascript
const csv = require('csv-parser');
```

Ahora, debemos leer un archivo CSV y procesar sus datos. Supongamos que queremos obtener el nombre y la edad de cada persona en nuestro archivo. Primero, debemos abrir el archivo utilizando el módulo "fs" de Node.js:

```javascript
const fs = require('fs');
const results = [];

fs.createReadStream('datos.csv')
  .pipe(csv())
  .on('data', (data) => results.push(data))
  .on('end', () => {
    // Aquí tenemos todos los datos del CSV almacenados en "results"
    // Ahora podemos procesarlos
    results.forEach((row) => {
      console.log(`Nombre: ${row.nombre}, Edad: ${row.edad}`);
    });
  });
```

La función "fs.createReadStream()" recibe el nombre de nuestro archivo CSV y lo convierte en un flujo de datos que podemos procesar. Luego, utilizamos la función "pipe()" para pasar el flujo de datos a la función "csv()" y convertirlo en un objeto que podamos manipular. Finalmente, utilizamos los eventos "data" y "end" para obtener los datos del CSV y procesarlos.

## Profundizando en el tema
Trabajar con archivos CSV en Javascript también puede incluir la escritura de datos en un archivo CSV, el manejo de valores nulos o vacíos, y la manipulación de tipos de datos. Puedes encontrar más información sobre estas y otras técnicas de procesamiento de CSV en la documentación oficial de "csv-parser": https://csv.js.org/

## Ver también
- https://www.npmjs.com/package/csv-parser
- https://csv.js.org/examples/using-csv.html
- https://nodejs.org/api/fs.html#fs_fs_createreadstream_path_options