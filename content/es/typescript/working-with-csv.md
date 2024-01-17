---
title:                "Trabajando con csv"
html_title:           "TypeScript: Trabajando con csv"
simple_title:         "Trabajando con csv"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/working-with-csv.md"
---

{{< edit_this_page >}}

## ¡Qué y Por Qué?

Trabajar con CSV (Comma Separated Values) se refiere a la manipulación de datos guardados en un formato de texto plano tabular, donde cada línea representa una "fila" de datos y cada valor está separado por una coma. Los programadores utilizan CSV porque es una forma simple y eficiente de almacenar grandes cantidades de datos en un formato legible por máquina.

## Cómo:

### Parsear un archivo CSV:

```TypeScript
import { readFileSync } from 'fs';

// Lee el archivo CSV como una cadena de texto
const csvData = readFileSync('ejemplo.csv', 'utf8');

// Separa el texto por filas
const filas = csvData.split('\n');

// Separa cada fila por comas y crea una matriz de valores
const matriz = filas.map(fila => fila.split(','));

// Imprime la matriz resultante
console.log(matriz);
```

Output: `[['Nombre', 'Edad', 'Ciudad'], ['Juan', '25', 'Madrid'], ['María', '30', 'Barcelona'], ['Luis', '33', 'Valencia']]`

### Escribir un archivo CSV:

```TypeScript
import { createWriteStream } from 'fs';

// Crea un stream de escritura para el nuevo archivo CSV
const csvStream = createWriteStream('nuevo.csv');

// Escribe la primera fila con los encabezados
csvStream.write('Nombre,Edad,Ciudad\n');

// Crear filas con datos
const persona = ['Ana', '28', 'Sevilla'];

// Convierte la fila en cadena, separando los valores por comas
const fila = persona.join(',');

//Escribe la fila en el stream
csvStream.write(fila);

// Cierra el stream
csvStream.end();
```

Output del archivo "nuevo.csv": `Nombre,Edad,Ciudad\nAna,28,Sevilla`

## Profundizando:

### Contexto Histórico:

El formato CSV se originó en los años 70 como una forma de intercambiar datos entre diferentes programas. A lo largo de los años, se ha convertido en un formato popular para la importación y exportación de datos, especialmente en aplicaciones de hojas de cálculo.

### Alternativas:

Existen varias alternativas al formato CSV, como JSON y XML, que también se utilizan para almacenar y manipular datos. Sin embargo, CSV sigue siendo una opción popular debido a su simplicidad y facilidad de lectura.

### Detalles de Implementación:

Al trabajar con CSV en TypeScript, es importante tener en cuenta que se trata de un formato de texto sin formato. Esto significa que los datos deben ser parseados y formateados de manera adecuada antes de ser utilizados por el programa. Además, se deben tener en cuenta posibles errores y caracteres especiales en los datos.

## Ver también:

- [Documentación de CSV en TypeScript](https://csv.js.org/parse/)
- [Especificaciones del formato CSV](https://tools.ietf.org/html/rfc4180)
- [Uso de CSV para importar y exportar datos en aplicaciones de hojas de cálculo](https://www.business2community.com/infographics/revealed-why-csv-files-are-the-best-file-format-for-spreadsheets-01288361)