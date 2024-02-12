---
title:                "Escribiendo en el error estándar"
date:                  2024-02-03T19:33:25.499262-07:00
model:                 gpt-4-0125-preview
simple_title:         "Escribiendo en el error estándar"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Qué y Por Qué
Escribir en el error estándar (stderr) en JavaScript se trata de dirigir mensajes de error o cualquier información crítica a un flujo específico y separado, lo cual es especialmente útil en entornos similares a Unix para propósitos de registro y depuración. Los programadores hacen esto para diferenciar la salida normal del programa de los mensajes de error, permitiendo una gestión de salida más limpia y un monitoreo de errores más sencillo.

## Cómo:
En Node.js, escribir en stderr se puede lograr utilizando el método `console.error()` o escribiendo directamente en `process.stderr`. Aquí hay ejemplos que demuestran ambos enfoques:

```javascript
// Usando console.error()
console.error('Este es un mensaje de error.');

// Escribiendo directamente en process.stderr
process.stderr.write('Este es otro mensaje de error.\n');
```

La salida de muestra para ambos métodos aparecería en el flujo de stderr, sin mezclarse con stdout:
```
Este es un mensaje de error.
Este es otro mensaje de error.
```

Para un registro más sofisticado o específico de la aplicación, muchos programadores de JavaScript utilizan bibliotecas de terceros como `winston` o `bunyan`. He aquí un rápido ejemplo usando `winston`:

Primero, instala `winston` vía npm:
```shell
npm install winston
```

Luego, configura `winston` para registrar errores en stderr:
```javascript
const winston = require('winston');

const logger = winston.createLogger({
  levels: winston.config.syslog.levels,
  transports: [
    new winston.transports.Console({
      stderrLevels: ['error']
    })
  ]
});

// Registrando un mensaje de error
logger.error('Error registrado a través de winston.');
```

Esta configuración asegura que cuando registras un error usando `winston`, este se dirige a stderr, ayudando a mantener una clara separación entre las salidas estándar y de error.
