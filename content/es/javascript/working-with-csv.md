---
title:                "Trabajando con csv"
html_title:           "Javascript: Trabajando con csv"
simple_title:         "Trabajando con csv"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/working-with-csv.md"
---

{{< edit_this_page >}}

¿Qué y por qué?

Trabajar con archivos CSV es muy común para los programadores. CSV significa "valores separados por comas" y se utiliza para almacenar datos en formato de tabla. Esto permite una fácil lectura y procesamiento de la información.

Los programadores utilizan archivos CSV para manipular grandes cantidades de datos estructurados, como listas de clientes, registros de ventas, informes financieros, entre otros. Los datos se pueden organizar, filtrar y analizar de manera más eficiente gracias al formato de tabla de CSV.

¿Cómo hacerlo?

A continuación, se muestra un ejemplo simple de cómo leer un archivo CSV y mostrar sus datos en la consola:

```Javascript
const fs = require('fs');
const csv = require('csv-parser');

fs.createReadStream('mydata.csv')
  .pipe(csv())
  .on('data', (data) => console.log(data))
  .on('end', () => console.log('Lectura de CSV finalizada.'));
```

Esta es la salida que obtendríamos en la consola:

```bash
{ name: 'Juan',
  age: '25',
  city: 'Madrid' }
{ name: 'Ana',
  age: '28',
  city: 'Barcelona' }
{ name: 'Carlos',
  age: '32',
  city: 'Valencia' }
```

Profundizando un poco más

CSV se ha utilizado desde la década de 1970 para importar y exportar datos de hojas de cálculo. Ha sido ampliamente aceptado como formato estándar para compartir datos entre aplicaciones. Alternativas como JSON o XML también se utilizan para almacenar datos estructurados, pero CSV es más ligero y simple de entender.

En la mayoría de los casos, los archivos CSV contienen datos en formato de texto plano, separados por comas. Sin embargo, también es posible tener otros delimitadores, como punto y coma o tabulaciones. Es importante tener en cuenta el tipo de delimitador que se está utilizando al leer o escribir un archivo CSV para evitar posibles errores.

Vea también

- [Documentación de Node.js para trabajar con archivos CSV](https://docs.nodejs.org/api/fs.html)
- [Ejemplo de cómo crear un archivo CSV con datos](https://stackabuse.com/reading-and-writing-csvs-in-node-js/)
- [Módulo CSV de NPM](https://www.npmjs.com/package/csv-parser)