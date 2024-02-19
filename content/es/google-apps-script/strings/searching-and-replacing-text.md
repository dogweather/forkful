---
aliases:
- /es/google-apps-script/searching-and-replacing-text/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:00:59.863044-07:00
description: "Buscar y reemplazar texto en Google Apps Script implica identificar\
  \ program\xE1ticamente cadenas espec\xEDficas en un documento, hoja de c\xE1lculo\
  \ o cualquier\u2026"
lastmod: 2024-02-18 23:09:09.488098
model: gpt-4-0125-preview
summary: "Buscar y reemplazar texto en Google Apps Script implica identificar program\xE1\
  ticamente cadenas espec\xEDficas en un documento, hoja de c\xE1lculo o cualquier\u2026"
title: Buscando y reemplazando texto
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Buscar y reemplazar texto en Google Apps Script implica identificar programáticamente cadenas específicas en un documento, hoja de cálculo o cualquier otro tipo de contenido de Google Apps, y sustituirlas con otros valores de texto. Los programadores utilizan esta funcionalidad para automatizar la edición de grandes volúmenes de contenido, corregir errores comunes, estandarizar la terminología a través de documentos o insertar datos dinámicos en plantillas.

## Cómo hacerlo:

Google Apps Script ofrece una manera directa de buscar y reemplazar texto, especialmente dentro de Google Docs y Sheets. A continuación, se muestran ejemplos para ambos.

### Google Docs:

Para buscar y reemplazar texto en un Documento de Google, interactuarás principalmente con la clase `DocumentApp`.

```javascript
function searchReplaceInDoc() {
  var doc = DocumentApp.getActiveDocument();
  var body = doc.getBody();
  
  // Para buscar y reemplazar una frase específica
  body.replaceText('searchText', 'replacementText');
  
  DocumentApp.getActiveDocument().saveAndClose();
}

// Uso
searchReplaceInDoc();
```

Este fragmento de código busca todas las ocurrencias de `'searchText'` en el Documento de Google activo y las reemplaza con `'replacementText'`.

### Google Sheets:

De manera similar, en Google Sheets, puedes usar `SpreadsheetApp` para realizar operaciones de búsqueda y reemplazo:

```javascript
function searchReplaceInSheet() {
  var sheet = SpreadsheetApp.getActiveSpreadsheet().getActiveSheet();
  
  // Buscar y reemplazar en la hoja activa actualmente
  // replaceText(searchText, replacement)
  sheet.createTextFinder('searchText').replaceAllWith('replacementText');
}

// Uso
searchReplaceInSheet();
```

En este ejemplo, `createTextFinder('searchText')` busca en la hoja activa 'searchText', y `replaceAllWith('replacementText')` reemplaza todas las ocurrencias con 'replacementText'.

## Análisis Profundo

La funcionalidad de buscar y reemplazar en Google Apps Script está muy influida por su naturaleza basada en la web, permitiendo que los scripts manipulen el texto a través de varias aplicaciones de Google sin problemas. Históricamente, esta capacidad proviene del contexto más amplio del procesamiento y manipulación de textos en la programación, donde expresiones regulares y funciones de cadena en lenguajes como Perl y Python establecen un alto estándar para la flexibilidad y el poder.

Aunque la funcionalidad de buscar y reemplazar de Google Apps Script es potente para sustituciones directas, carece de las capacidades completas de expresiones regulares que se encuentran en algunos otros lenguajes. Por ejemplo, aunque puedes usar expresiones regulares básicas en `createTextFinder` en Google Sheets, las opciones para coincidencia de patrones complejos y manipulación son limitadas en comparación con Perl o Python.

Para necesidades de procesamiento de texto más avanzadas, los programadores podrían recurrir a exportar el contenido de Google Docs o Sheets a un formato que pueda ser procesado externamente con lenguajes más poderosos o emplear Google Apps Script para llamar a APIs o servicios externos que ofrezcan capacidades de manipulación de texto más sofisticadas.

A pesar de estas limitaciones, para la mayoría de las tareas típicas de buscar y reemplazar dentro del ecosistema de Google Apps, Google Apps Script ofrece una solución simple, eficiente y altamente integrable adaptada a las necesidades de automatización y scripting dentro del conjunto de herramientas de productividad de Google.
