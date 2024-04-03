---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:04:19.664601-07:00
description: "C\xF3mo: Usar expresiones regulares en Google Apps Script es sencillo\
  \ gracias a la sintaxis basada en JavaScript. Aqu\xED te mostramos c\xF3mo puedes\
  \ incorporar\u2026"
lastmod: '2024-03-13T22:44:58.513161-06:00'
model: gpt-4-0125-preview
summary: Usar expresiones regulares en Google Apps Script es sencillo gracias a la
  sintaxis basada en JavaScript.
title: Usando expresiones regulares
weight: 11
---

## Cómo:
Usar expresiones regulares en Google Apps Script es sencillo gracias a la sintaxis basada en JavaScript. Aquí te mostramos cómo puedes incorporar regex en tus scripts para tareas comunes como búsqueda y validación de datos.

### Buscando en Cadenas
Supongamos que quieres encontrar si una cadena contiene un patrón específico, como una dirección de correo electrónico. Aquí hay un ejemplo simple:

```javascript
function findEmailInText(text) {
  var emailPattern = /\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Z|a-z]{2,}\b/;
  var encontrado = text.match(emailPattern);
  if (encontrado) {
    Logger.log("Encontrado: " + encontrado[0]);
  } else {
    Logger.log("No se encontró ningún correo electrónico.");
  }
}

// Uso de muestra
findEmailInText("Contáctanos en info@example.com.");
```

### Validación de Datos
Las expresiones regulares brillan en la validación de datos. A continuación, una función que valida una cadena de entrada para verificar si se adhiere a una política de contraseña simple (al menos una letra mayúscula, una letra minúscula y un mínimo de 8 caracteres).

```javascript
function validatePassword(password) {
  var passwordPattern = /^(?=.*[a-z])(?=.*[A-Z]).{8,}$/;
  return passwordPattern.test(password);
}

// Salida de muestra
Logger.log(validatePassword("Str0ngPass")); // Salida: true
Logger.log(validatePassword("weak"));       // Salida: false
```

## Inmersión Profunda
Las expresiones regulares en Google Apps Script se heredan de JavaScript, estandarizadas por primera vez en la especificación del lenguaje ECMAScript en junio de 1997. Aunque poderosas, a veces pueden llevar a código confuso y difícil de mantener, especialmente cuando se usan en exceso o para tareas complejas de coincidencia de patrones que podrían resolverse de manera más eficiente a través de otros métodos de análisis.

Por ejemplo, aunque puedes usar regex para el análisis de HTML o XML en un apuro, hacerlo generalmente no se recomienda debido a las estructuras anidadas y complejas de estos documentos. En su lugar, las herramientas diseñadas específicamente para analizar tales estructuras, como los analizadores DOM para HTML, son más fiables y legibles.

Además, los desarrolladores de Google Apps Script deben ser conscientes de posibles problemas de rendimiento al usar patrones regex complejos en tareas de manipulación de texto a gran escala, ya que el procesamiento de regex puede ser intensivo en CPU. En tales casos, dividir la tarea en sub-tareas más simples o usar funciones integradas de manipulación de cadenas podría ofrecer un mejor equilibrio entre rendimiento y mantenibilidad.
