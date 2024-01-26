---
title:                "Trabajando con archivos CSV"
html_title:           "Bash: Trabajando con archivos CSV"
simple_title:         "Trabajando con archivos CSV"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/working-with-csv.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Trabajar con CSV (valores separados por comas) significa manejar datos en un formato de texto sencillo, donde cada línea representa un registro con campos divididos por comas. Los programadores lo usan por su simplicidad y compatibilidad con múltiples herramientas y lenguajes de programación.

## Cómo hacerlo:

Para manejar CSV en JavaScript, primero leeremos un archivo CSV y luego lo convertiremos en un arreglo de objetos JSON.

```javascript
const fs = require('fs');
const parse = require('csv-parse/lib/sync');

// Leer archivo CSV
const input = fs.readFileSync('datos.csv', 'utf8');

// Convertir CSV a JSON
const records = parse(input, {
  columns: true,
  skip_empty_lines: true
});

console.log(records);
```

Suponiendo que `datos.csv` tiene el siguiente contenido:
```
nombre,edad,ciudad
Juan,30,Madrid
Lucía,25,Sevilla
```

La salida sería un arreglo de objetos JavaScript:
```javascript
[
  { nombre: 'Juan', edad: '30', ciudad: 'Madrid' },
  { nombre: 'Lucía', edad: '25', ciudad: 'Sevilla' }
]
```

## Profundizando

El formato CSV tiene raíces en los años 70 cuando era usado en programas antiguos para transferir datos. Aunque existen formatos alternativos, como JSON o XML, CSV permanece relevante por su simplicidad y la capacidad de ser editado fácilmente en un editor de texto o hojas de cálculo.

Respecto a la implementación, programas Node.js a menudo emplean paquetes como `csv-parse` para parsear datos CSV. Si bien JavaScript puro en un navegador no soporta operaciones de archivos por razones de seguridad, la API `File` puede ser usada para cargar y leer archivos CSV en entornos de cliente.

## Ver También:

- Documentación de Node.js sobre el sistema de archivos (fs): https://nodejs.org/api/fs.html
- Repositorio npm de `csv-parse`: https://www.npmjs.com/package/csv-parse
- API `File` de JavaScript: https://developer.mozilla.org/en-US/docs/Web/API/File/Using_files_from_web_applications
- CSV en Wikipedia: https://es.wikipedia.org/wiki/Valores_separados_por_comas
