---
title:                "Obteniendo la fecha actual"
aliases: - /es/typescript/getting-the-current-date.md
date:                  2024-02-03T19:10:54.323474-07:00
model:                 gpt-4-0125-preview
simple_title:         "Obteniendo la fecha actual"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Obtener la fecha actual en TypeScript, un lenguaje construido sobre JavaScript, te permite acceder y manipular la información de la fecha y hora actuales. Los programadores a menudo necesitan esta funcionalidad para crear marcas de tiempo, programar y otras características sensibles al tiempo en sus aplicaciones.

## Cómo hacerlo:
En TypeScript, puedes usar el objeto `Date` para obtener la fecha y hora actuales. Así es cómo puedes hacerlo:

```typescript
const currentDate = new Date();
console.log(currentDate);
```

Salida de muestra:
```
2023-04-12T07:20:50.52Z
```

Este fragmento de código crea un nuevo objeto `Date` que contiene la fecha y hora actuales, que luego se imprime en la consola. También puedes formatear la fecha usando toLocaleDateString() para formatos más legibles:

```typescript
const currentDate = new Date();
console.log(currentDate.toLocaleDateString());
```

Salida de muestra:
```
12/4/2023
```

### Usando date-fns
Para una manipulación y formato de fechas más extensos, la biblioteca `date-fns` es una opción popular. Primero, instálala a través de npm:

```bash
npm install date-fns
```

Luego, puedes usarla para formatear la fecha actual:

```typescript
import { format } from 'date-fns';

const currentDate = new Date();
console.log(format(currentDate, 'yyyy-MM-dd'));
```

Salida de muestra:
```
2023-04-12
```

Este ejemplo de `date-fns` formatea la fecha actual como una cadena en el formato "AAAA-MM-DD". La biblioteca ofrece una plétora de funciones para la manipulación de fechas, convirtiéndola en una herramienta versátil para cualquier programador de TypeScript que trabaje con fechas.
