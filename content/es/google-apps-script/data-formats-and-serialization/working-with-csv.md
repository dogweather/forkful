---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:05:01.711170-07:00
description: "C\xF3mo hacerlo: #."
lastmod: '2024-03-13T22:44:58.589187-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: Trabajando con CSV
weight: 37
---

## Cómo hacerlo:


### Leer datos CSV
Para leer datos CSV de un archivo almacenado en Google Drive, primero necesitas obtener el contenido del archivo como una cadena, y luego analizarlo. Google Apps Script facilita la obtención del contenido del archivo con el servicio DriveApp.

```javascript
function readCSV() {
  var fileId = 'YOUR_FILE_ID_HERE'; // Reemplazar con el ID de archivo real
  var file = DriveApp.getFileById(fileId);
  var content = file.getBlob().getDataAsString();
  var rows = content.split("\n");
  
  for (var i = 0; i < rows.length; i++) {
    var cells = rows[i].split(",");
    Logger.log(cells); // Registrar las celdas de cada fila
  }
}
```

### Escribir datos CSV
Crear y escribir en un CSV implica construir una cadena con valores separados por comas y nuevas líneas, luego guardar o exportarla. Este ejemplo demuestra cómo crear un nuevo archivo CSV en Google Drive.

```javascript
function writeCSV() {
  var folderId = 'YOUR_FOLDER_ID_HERE'; // Reemplazar con el ID de la carpeta de Drive donde se creará el nuevo archivo
  var csvContent = "Name,Age,Occupation\nJohn Doe,29,Engineer\nJane Smith,34,Designer";
  var fileName = "example.csv";
  
  var folder = DriveApp.getFolderById(folderId);
  folder.createFile(fileName, csvContent, MimeType.PLAIN_TEXT);
}
```

### Muestra de salida
Al registrar las celdas de filas al leer un CSV:

```plaintext
[John, 29, Ingeniero]
[Jane, 34, Diseñadora]
```

Al escribir, se crea un archivo nombrado "example.csv" con el contenido:

```plaintext
Name,Age,Occupation
John Doe,29,Engineer
Jane Smith,34,Designer
```

## Análisis Profundo
Históricamente, los archivos CSV han sido favorecidos por su simplicidad y legibilidad humana, haciéndolos accesibles para no programadores y útiles para tareas rápidas de inspección de datos. Sin embargo, Google Apps Script opera dentro del ámbito del ecosistema de Google, donde Google Sheets actúa como una alternativa potente y amigable para la manipulación de CSV. Sheets no solo proporciona una GUI para editar datos, sino que también soporta fórmulas complejas, estilos y muchas más características que los CSV en crudo carecen.

A pesar de las ventajas ofrecidas por Google Sheets, la manipulación directa de CSV en Google Apps Script sigue siendo importante para tareas automatizadas, especialmente cuando se trata de sistemas externos que generan o requieren datos en formato CSV. Por ejemplo, integrarse con sistemas heredados, exportar datos para usar en otras aplicaciones o preprocesamiento antes de alimentar datos en Google Sheets.

Además, la capacidad de Google Apps Script para trabajar con archivos CSV puede extenderse con el servicio Utilities para necesidades avanzadas de codificación, o interfazarse con APIs externas para tareas de conversión, análisis o validación. Sin embargo, para trabajar con grandes conjuntos de datos o requerir manipulaciones complejas, considere aprovechar las APIs de Google Sheets o explorar BigQuery para capacidades de procesamiento de datos más robustas.

Mientras que la simplicidad sigue siendo una razón clave para la popularidad de CSV, estas alternativas ofrecen un conjunto más rico de características para tratar con datos en el extenso ecosistema de Google Cloud.
