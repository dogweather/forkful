---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:29.624815-07:00
description: "C\xF3mo hacerlo: TypeScript, al ser un superconjunto de JavaScript,\
  \ depende del entorno de ejecuci\xF3n subyacente de JS (como Node.js) para escribir\
  \ en stderr.\u2026"
lastmod: '2024-03-13T22:44:58.819027-06:00'
model: gpt-4-0125-preview
summary: "TypeScript, al ser un superconjunto de JavaScript, depende del entorno de\
  \ ejecuci\xF3n subyacente de JS (como Node.js) para escribir en stderr."
title: "Escribiendo en el error est\xE1ndar"
weight: 25
---

## Cómo hacerlo:
TypeScript, al ser un superconjunto de JavaScript, depende del entorno de ejecución subyacente de JS (como Node.js) para escribir en stderr. Aquí te mostramos cómo puedes hacerlo directamente:

```typescript
console.error("Este es un mensaje de error.");
```

Ejemplo de salida a stderr:
```
Este es un mensaje de error.
```

En un entorno Node.js, también puedes usar el método `process.stderr.write()` para escribir a un nivel más bajo:

```typescript
process.stderr.write("Mensaje de error a bajo nivel.\n");
```

Ejemplo de salida a stderr:
```
Mensaje de error a bajo nivel.
```

Para un registro de errores más estructurado, podrías usar bibliotecas de terceros populares como `winston` o `pino`. Aquí te mostramos cómo registrar errores usando `winston`:

Primero, instala `winston`:

```bash
npm install winston
```

Luego úsalo en tu archivo TypeScript:

```typescript
import * as winston from 'winston';

const logger = winston.createLogger({
  levels: winston.config.syslog.levels,
  transports: [
    new winston.transports.Console(),
    new winston.transports.File({ filename: 'error.log', level: 'error' })
  ],
});

logger.error('Error registrado usando winston.');
```

Esto escribirá el error tanto en la consola como en un archivo llamado `error.log`. Recuerda, cuando escribes en archivos, es importante gestionar los permisos de archivo y el rollover para prevenir problemas relacionados con el uso del espacio en disco.
