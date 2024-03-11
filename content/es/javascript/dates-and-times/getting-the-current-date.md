---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:43.174356-07:00
description: "Obtener la fecha actual en JavaScript es una tarea fundamental, que\
  \ implica recuperar y posiblemente manipular la fecha y hora de hoy. Los programadores\u2026"
lastmod: '2024-03-11T00:14:33.301349-06:00'
model: gpt-4-0125-preview
summary: "Obtener la fecha actual en JavaScript es una tarea fundamental, que implica\
  \ recuperar y posiblemente manipular la fecha y hora de hoy. Los programadores\u2026"
title: Obteniendo la fecha actual
---

{{< edit_this_page >}}

## Qué y Por Qué?
Obtener la fecha actual en JavaScript es una tarea fundamental, que implica recuperar y posiblemente manipular la fecha y hora de hoy. Los programadores realizan esto para mostrar fechas en sitios web, en aplicaciones, para rastrear interacciones de usuarios, o para manejar datos sensibles al tiempo.

## Cómo hacerlo:
En JavaScript puro (vanilla), el objeto `Date` se utiliza para trabajar con fechas y horas. Así es como puedes obtener la fecha y hora actuales:

```javascript
const currentDate = new Date();
console.log(currentDate); // Salida de ejemplo: Fri Apr 14 2023 12:34:56 GMT+0100 (Hora de Verano Británica)
```

Para mostrar solo la fecha en un formato más amigable para el usuario, puedes usar métodos como `toLocaleDateString()`:

```javascript
console.log(currentDate.toLocaleDateString()); // Salida de ejemplo: 14/4/2023
```

Para tener más control sobre el formato, bibliotecas de terceros como *Moment.js* o *date-fns* son muy populares, aunque es bueno saber que Moment.js ahora se considera un proyecto legado en modo de mantenimiento.

Usando *Moment.js*:

```javascript
const moment = require('moment'); // asumiendo Node.js o usando un paquete de módulos
const formattedDate = moment().format('YYYY-MM-DD');
console.log(formattedDate); // Salida de ejemplo: 2023-04-14
```

Con *date-fns*, que enfatiza la modularización permitiéndote importar solo lo que necesitas:

```javascript
const { format } = require('date-fns');
const formattedDate = format(new Date(), 'yyyy-MM-dd');
console.log(formattedDate); // Salida de ejemplo: 2023-04-14
```

Cada enfoque ofrece diferentes niveles de conveniencia y flexibilidad para trabajar con fechas en JavaScript, desde el objeto `Date` incorporado hasta capacidades de formato y manipulación más sofisticadas disponibles a través de bibliotecas.
