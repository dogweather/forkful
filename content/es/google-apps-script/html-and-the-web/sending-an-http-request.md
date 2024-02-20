---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:01:15.609528-07:00
description: "Enviar una solicitud HTTP en Google Apps Script se trata de hacer una\
  \ llamada program\xE1tica a un servidor web externo o API. Los programadores hacen\
  \ esto\u2026"
lastmod: 2024-02-19 22:05:17.157873
model: gpt-4-0125-preview
summary: "Enviar una solicitud HTTP en Google Apps Script se trata de hacer una llamada\
  \ program\xE1tica a un servidor web externo o API. Los programadores hacen esto\u2026"
title: Enviando una solicitud HTTP
---

{{< edit_this_page >}}

## Qué y Por Qué?

Enviar una solicitud HTTP en Google Apps Script se trata de hacer una llamada programática a un servidor web externo o API. Los programadores hacen esto para recuperar o enviar datos a servicios web, integrando un vasto reino de recursos web y funcionalidades directamente en sus proyectos de Google Apps Script.

## Cómo:

En Google Apps Script, la forma principal de enviar una solicitud HTTP es mediante el uso del servicio `UrlFetchApp`. Este servicio proporciona métodos para hacer solicitudes HTTP GET y POST. Aquí hay un ejemplo simple de cómo hacer una solicitud GET para recuperar datos JSON:

```javascript
function fetchJsonData() {
  var url = 'https://api.example.com/data';
  var response = UrlFetchApp.fetch(url);
  var json = response.getContentText();
  var data = JSON.parse(json);
  
  Logger.log(data);
}
```

Para una solicitud POST, que se usa comúnmente para enviar datos a un servidor, necesitas incluir más detalles en el parámetro de opciones:

```javascript
function postExample() {
  var url = 'https://api.example.com/post';
  var payload = {
    key1: 'value1',
    key2: 'value2'
  };
  
  var options = {
    'method' : 'post',
    'contentType': 'application/json',
    // Convertir el objeto JavaScript a una cadena JSON
    'payload' : JSON.stringify(payload)
  };
  
  var response = UrlFetchApp.fetch(url, options);
  Logger.log(response.getContentText());
}
```

Estos fragmentos muestran implementaciones básicas de solicitudes GET y POST. La salida dependerá de la respuesta de la API y se puede ver en el Logger de Google Apps Script.

## Análisis Profundo

El servicio `UrlFetchApp` de Google Apps Script ha evolucionado significativamente desde su inicio, ofreciendo un control más matizado sobre las solicitudes HTTP con características como establecer encabezados, carga de datos (payload) y manejo de multipart/form-data para la subida de archivos. Aunque proporciona un medio sencillo para integrar servicios web externos, los desarrolladores que provienen de lenguajes de backend más robustos pueden encontrar su funcionalidad algo limitante en comparación con bibliotecas como `requests` de Python o la API `fetch` de JavaScript en Node.js.

Una limitación notable es el límite de tiempo de ejecución para Google Apps Script, que afecta a las solicitudes de larga duración. Además, aunque `UrlFetchApp` cubre una amplia gama de casos de uso, los escenarios más complejos que involucran autenticación OAuth o el manejo de cargas útiles muy grandes pueden requerir soluciones creativas o aprovechar recursos adicionales de Google Cloud.

No obstante, para la mayoría de las integraciones que los desarrolladores de Google Workspace encuentran—desde automatizar la recuperación de datos hasta publicar actualizaciones en servicios externos—`UrlFetchApp` proporciona una herramienta potente y accesible. Su integración en Google Apps Script significa que no hay necesidad de bibliotecas externas o configuraciones complejas, haciendo que las solicitudes HTTP sean relativamente sencillas de ejecutar dentro de las limitaciones de Google Apps Script. A medida que el panorama de las API web sigue expandiéndose, `UrlFetchApp` sigue siendo un puente crítico para que los programas de Google Apps Script interactúen con el mundo más allá del ecosistema de Google.
