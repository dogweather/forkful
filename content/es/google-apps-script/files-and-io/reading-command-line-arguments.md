---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:58:49.858889-07:00
description: "Leer argumentos de la l\xEDnea de comandos en Google Apps Script es\
  \ un poco un t\xE9rmino incorrecto porque, a diferencia de las interfaces de l\xED\
  nea de comandos\u2026"
lastmod: '2024-03-13T22:44:58.568380-06:00'
model: gpt-4-0125-preview
summary: "Leer argumentos de la l\xEDnea de comandos en Google Apps Script es un poco\
  \ un t\xE9rmino incorrecto porque, a diferencia de las interfaces de l\xEDnea de\
  \ comandos tradicionales en lenguajes de programaci\xF3n como Python o Node."
title: "Leyendo argumentos de la l\xEDnea de comandos"
weight: 23
---

## ¿Qué y Por Qué?

Leer argumentos de la línea de comandos en Google Apps Script es un poco un término incorrecto porque, a diferencia de las interfaces de línea de comandos tradicionales en lenguajes de programación como Python o Node.js, Google Apps Script no admite inherentemente la ejecución de la línea de comando o el análisis de argumentos. En cambio, los programadores a menudo simulan este proceso a través de funciones personalizadas y parámetros de URL al ejecutar aplicaciones web o tareas automatizadas, lo que permite la interacción dinámica con las funcionalidades del script basado en entradas de usuarios o parámetros predefinidos.

## Cómo hacerlo:

Para imitar el proceso de leer argumentos de la línea de comandos en Google Apps Script, particularmente para aplicaciones web, puedes utilizar parámetros de cadena de consulta. Cuando un usuario accede a la URL de la aplicación web, puedes agregar argumentos como `?name=John&age=30` y analizar estos dentro de tu código de Apps Script. Así es como podrías configurarlo:

```javascript
function doGet(e) {
  var params = e.parameter; // Recupera los parámetros de la cadena de consulta
  var name = params['name']; // Obtiene el parámetro 'name'
  var age = params['age']; // Obtiene el parámetro 'age'

  // Ejemplo de salida:
  var output = "Name: " + name + ", Age: " + age;
  return HtmlService.createHtmlOutput(output);
}

// Ejemplo de URL: https://script.google.com/macros/s/tu_id_de_script/exec?name=John&age=30
```

Cuando accedes a la URL con los parámetros especificados, el script produce algo como:

```
Name: John, Age: 30
```

Este enfoque es instrumental para crear interacciones personalizadas en aplicaciones web o controlar programáticamente las ejecuciones de scripts.

## Análisis Profundo

Arguments de línea de comandos, como se entiende en el contexto de los lenguajes de programación tradicionales, traen las capacidades para que los scripts y aplicaciones procesen parámetros en tiempo de ejecución, permitiendo así ejecuciones de código flexibles y dinámicas basadas en la entrada del usuario o procesos automatizados. Google Apps Script, siendo un lenguaje de scripting basado en la nube para el desarrollo de aplicaciones ligeras en el ecosistema de Google Workspace, no opera nativamente a través de una interfaz de línea de comandos. En cambio, su ejecución es en gran parte impulsada por eventos o activada manualmente a través de la interfaz de usuario de Apps Script y Google Workspace, o mediante aplicaciones web que pueden analizar parámetros de URL como argumentos de línea de comandos pseudo.

Dada esta diferencia arquitectónica, los programadores que vienen de un fondo de lenguajes con intenso uso de CLI podrían necesitar ajustar su enfoque cuando automatizan tareas o desarrollan aplicaciones en Google Apps Script. En lugar de análisis de argumentos de línea de comandos tradicionales, aprovechar la funcionalidad de aplicación web de Google Apps Script o incluso funciones personalizadas de Google Sheets para el procesamiento interactivo de datos puede servir a fines similares. Aunque esto podría parecer una limitación al principio, fomenta el desarrollo de interfaces más amigables para el usuario y aplicaciones web accesibles, alineándose con el enfoque de Google Apps Script en la integración y extensión sin fisuras de aplicaciones de Google Workspace.

Para escenarios donde la emulación cercana del comportamiento de CLI es primordial (por ejemplo, automatizar tareas con parámetros dinámicos), los desarrolladores podrían explorar el aprovechamiento de plataformas externas que llaman a aplicaciones web de Google Apps Script, pasando parámetros a través de URLs como un método "de línea de comandos" improvisado. Sin embargo, para proyectos nativos de Google Apps Script, adoptar el modelo impulsado por eventos y centrado en la UI de la plataforma a menudo conduce a soluciones más simples y mantenibles.
