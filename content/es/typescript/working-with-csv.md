---
title:                "Trabajando con archivos CSV"
date:                  2024-01-19
html_title:           "Bash: Trabajando con archivos CSV"
simple_title:         "Trabajando con archivos CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/working-with-csv.md"
---

{{< edit_this_page >}}

## Qué es y por qué?
Trabajar con CSV implica manipular archivos del formato “Valores Separados por Comas”, que es común para datos tabulares. Los programadores lo hacen porque es un formato simple, ampliamente soportado y fácil de integrar con hojas de cálculo y bases de datos.

## Cómo hacerlo:
```TypeScript
import fs from 'fs';
import parse from 'csv-parse/lib/sync';

// Leer archivo CSV
const input = fs.readFileSync('./datos.csv', 'utf-8');

// Convertir CSV a JSON
const records = parse(input, {
  columns: true,
  skip_empty_lines: true
});

// Uso de los datos
records.forEach((record) => {
  console.log(`Nombre: ${record.Nombre}, Edad: ${record.Edad}`);
});
```
Salida de ejemplo:
```
Nombre: Juan, Edad: 30
Nombre: Ana, Edad: 25
...
```

## Análisis profundo:
El formato CSV existe desde los primeros días de las computadoras personales. Alternativas como XML y JSON ofrecen estructuras más complejas, pero el CSV sigue siendo popular por su simplicidad. En TypeScript, puedes implementar la manipulación de CSV con librerías como `csv-parse` para lectura o `csv-stringify` para escritura, lo cual facilita trabajar con archivos CSV de una manera más tipada y segura.

## Ver también:
- Documentación sobre `csv-parse`: https://csv.js.org/parse/
- Guía de Node.js para trabajar con archivos del sistema: https://nodejs.dev/learn/reading-files-with-nodejs
- Convertir CSV a JSON en línea: https://csvjson.com/csv2json
