---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:25.499262-07:00
description: "Escribir en el error est\xE1ndar (stderr) en JavaScript se trata de\
  \ dirigir mensajes de error o cualquier informaci\xF3n cr\xEDtica a un flujo espec\xED\
  fico y\u2026"
lastmod: '2024-03-13T22:44:59.474675-06:00'
model: gpt-4-0125-preview
summary: "Escribir en el error est\xE1ndar (stderr) en JavaScript se trata de dirigir\
  \ mensajes de error o cualquier informaci\xF3n cr\xEDtica a un flujo espec\xEDfico\
  \ y separado, lo cual es especialmente \xFAtil en entornos similares a Unix para\
  \ prop\xF3sitos de registro y depuraci\xF3n."
title: "Escribiendo en el error est\xE1ndar"
weight: 25
---

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
