---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:54:12.719471-07:00
description: "Obtener la fecha actual en Google Apps Script se trata de obtener la\
  \ fecha y hora en vivo, una tarea com\xFAn para automatizar tareas, registrar y\
  \ poner\u2026"
lastmod: '2024-03-13T22:44:58.547526-06:00'
model: gpt-4-0125-preview
summary: "Obtener la fecha actual en Google Apps Script se trata de obtener la fecha\
  \ y hora en vivo, una tarea com\xFAn para automatizar tareas, registrar y poner\
  \ marcas de tiempo en aplicaciones vinculadas al ecosistema de Google."
title: Obteniendo la fecha actual
weight: 29
---

## Cómo:
Google Apps Script, que se basa en JavaScript, ofrece métodos sencillos para obtener la fecha actual. Puedes usar el constructor `new Date()` para crear un nuevo objeto de fecha que represente la fecha y hora actuales. Así es como puedes manipular y mostrar esto en varios formatos.

```javascript
function mostrarFechaActual() {
  var currentDate = new Date();
  
  Logger.log(currentDate); // Registra la fecha y hora actuales en la zona horaria del script
  
  // Para mostrar solo la fecha en formato AAAA-MM-DD
  var dateString = currentDate.getFullYear() + '-' + 
                   (currentDate.getMonth() + 1).toString().padStart(2, '0') + '-' + 
                   currentDate.getDate().toString().padStart(2, '0');
  Logger.log(dateString); // Ejemplo de salida: "2023-04-01"
  
  // Mostrando en un formato más legible
  var options = { year: 'numeric', month: 'long', day: 'numeric', hour: '2-digit', minute: '2-digit', second: '2-digit', timeZoneName: 'short' };
  var readableDate = currentDate.toLocaleDateString('en-US', options) + ' ' + 
                     currentDate.toLocaleTimeString('en-US', options);
                     
  Logger.log(readableDate); // Ejemplo de salida: "April 1, 2023, 12:00:00 PM GMT+1"
}
```

Estos fragmentos demuestran cómo capturar y formatear la fecha y hora actuales, mostrando la versatilidad para diversas necesidades de programación dentro de Google Apps Script.

## Estudio Detallado
Antes de que JavaScript se decidiera por el objeto `Date`, los programadores tenían que seguir manualmente la hora y la fecha a través de medios menos estándar y más engorrosos. Esto incluía el uso de enteros de marca de tiempo y funciones de fecha caseras, que variaban de un entorno de programación a otro, lo que llevaba a inconsistencias y problemas de compatibilidad.

La introducción del objeto `new Date()` en JavaScript, y por extensión en Google Apps Script, estandarizó las operaciones de fecha y hora, haciéndolas más intuitivas y reduciendo la cantidad de código necesario para operaciones relacionadas con la fecha. Vale la pena notar que, si bien la implementación de Google Apps Script es conveniente y suficiente para muchas aplicaciones dentro del conjunto de productos de Google, puede no atender a todos los escenarios, especialmente aquellos que requieren manejo complejo de zonas horarias o registro de marcas de tiempo preciso en entornos de ritmo rápido.

Para tales casos de uso avanzados, los programadores a menudo recurren a bibliotecas como Moment.js o date-fns en JavaScript. Aunque Google Apps Script no admite de forma nativa estas bibliotecas, los desarrolladores pueden imitar algunas de sus funcionalidades usando los métodos de fecha de JavaScript disponibles o accediendo a bibliotecas externas a través del Servicio HTML o el servicio URL Fetch de Apps Script. A pesar de estas alternativas, la simplicidad e integración de las funciones nativas de fecha y hora de Google Apps Script siguen siendo la opción preferencial para la mayoría de las tareas del ecosistema de Google.
