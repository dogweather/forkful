---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:56:25.923473-07:00
description: "C\xF3mo hacerlo: En Google Apps Script, que se basa en JavaScript, se\
  \ definen las funciones utilizando la palabra clave `function`, seguida por un nombre\
  \ de\u2026"
lastmod: '2024-03-13T22:44:58.539408-06:00'
model: gpt-4-0125-preview
summary: "En Google Apps Script, que se basa en JavaScript, se definen las funciones\
  \ utilizando la palabra clave `function`, seguida por un nombre de funci\xF3n \xFA\
  nico, par\xE9ntesis `()` que pueden contener par\xE1metros y llaves `{}` que encapsulan\
  \ el bloque de c\xF3digo de la funci\xF3n."
title: "Organizando c\xF3digo en funciones"
weight: 18
---

## Cómo hacerlo:
En Google Apps Script, que se basa en JavaScript, se definen las funciones utilizando la palabra clave `function`, seguida por un nombre de función único, paréntesis `()` que pueden contener parámetros y llaves `{}` que encapsulan el bloque de código de la función. Aquí hay un ejemplo básico:

```javascript
function saludarUsuario() {
  var usuario = Session.getActiveUser().getEmail();
  Logger.log('Hola, ' + usuario + '!');
}

saludarUsuario();
```

Salida de muestra:

```
Hola, alguien@example.com!
```

Ahora, consideremos un ejemplo más práctico relacionado con Google Sheets donde separamos la funcionalidad en dos funciones: una para configurar la hoja y otra para llenarla con datos.

```javascript
function configurarHoja() {
  var ss = SpreadsheetApp.getActiveSpreadsheet();
  var hoja = ss.getSheets()[0];
  hoja.setName('Datos de Ventas');
  hoja.appendRow(['Artículo', 'Cantidad', 'Precio']);
}

function poblarHoja(datos) {
  var hoja = SpreadsheetApp.getActiveSpreadsheet().getSheetByName('Datos de Ventas');
  datos.forEach(function(fila) {
    hoja.appendRow(fila);
  });
}

// Inicializar array de datos
var datosDeVentas = [
  ['Widgets', 15, 2.5],
  ['Gadgets', 8, 3.75]
];

// Ejecutar las funciones
configurarHoja();
poblarHoja(datosDeVentas);
```

En este ejemplo, `configurarHoja` prepara la hoja, y `poblarHoja` toma un array de datos de ventas para llenar la hoja. Separar estas preocupaciones hace que el código sea más limpio y más adaptable a los cambios.

## Análisis Profundo
El concepto de dividir el código en funciones no es nuevo ni único de Google Apps Script; es una práctica de programación fundamental defendida en casi todos los lenguajes de programación. Históricamente, las funciones evolucionaron del concepto matemático de mapear entradas a salidas, lo cual se convirtió en una piedra angular en la programación estructurada. Este enfoque promueve la modularidad y reutilización de código, ofreciendo caminos claros para probar partes individuales del script.

Google Apps Script, al estar basado en JavaScript, se beneficia significativamente de las funciones de primera clase de JavaScript, lo que permite que las funciones se pasen como argumentos, se devuelvan de otras funciones y se asignen a variables. Esta característica abre patrones avanzados como callbacks y programación funcional, aunque estos patrones pueden introducir complejidad que podría ser innecesaria para tareas simples de automatización en Google Apps Script.

Para proyectos más grandes o aplicaciones más complejas, los desarrolladores podrían explorar el uso de características más nuevas de JavaScript como funciones de flecha, async/await para operaciones asíncronas e incluso TypeScript para tipado estático. TypeScript, en particular, se puede compilar para ejecutarse como Google Apps Script, brindando una vía para desarrolladores que buscan una verificación de tipo más robusta y características orientadas a objetos más avanzadas.

Sin embargo, para la mayoría de las necesidades de scripting dentro de Google Apps suite, apegarse a funciones simples y bien organizadas como se demostró proporciona una base sólida. Siempre es un acto de equilibrio entre aprovechar las características avanzadas para la eficiencia y mantener la simplicidad para facilitar el mantenimiento y la legibilidad.
