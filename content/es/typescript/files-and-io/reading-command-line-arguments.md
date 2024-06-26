---
date: 2024-01-20 17:57:13.979254-07:00
description: "C\xF3mo: Salida de muestra si ejecutas `ts-node tu_script.ts hola mundo`."
lastmod: '2024-04-05T21:54:00.165649-06:00'
model: gpt-4-1106-preview
summary: Salida de muestra si ejecutas `ts-node tu_script.ts hola mundo`.
title: "Lectura de argumentos de l\xEDnea de comandos"
weight: 23
---

## Cómo:
```typescript
// Dependencias
import process from 'process';

// Capta los argumentos de la línea de comandos, excluyendo los primeros dos valores
const args = process.argv.slice(2);

// Uso de los argumentos
console.log('Argumentos recibidos:');
args.forEach((arg, index) => {
  console.log(`[${index}]: ${arg}`);
});

// Ejecuta esto con: ts-node tu_script.ts arg1 arg2
```

Salida de muestra si ejecutas `ts-node tu_script.ts hola mundo`:

```
Argumentos recibidos:
[0]: hola
[1]: mundo
```

## Análisis Profundo:
Los argumentos de la línea de comandos son una tradición de la vieja escuela, cuando los terminales eran la principal interfaz de usuario. En Node.js, `process.argv` es el estándar para acceder a ellos. Otros lenguajes tienen mecanismos similares, como `sys.argv` en Python o `$ARGV` en Perl.

Alternativas modernas para manejar argumentos en Node.js incluyen bibliotecas como yargs o commander, que ofrecen más funcionalidades como parsing inteligente, flags y comandos nombrados.

La implementación típica con `process.argv` es directa: es un array donde los dos primeros elementos son la ruta a Node.js y al script, por eso usamos `.slice(2)` para obtener únicamente los argumentos relevantes.

## Ver También:
- Documentación de Node.js sobre `process.argv`: https://nodejs.org/docs/latest/api/process.html#process_process_argv
- Biblioteca yargs para manejar argumentos: https://www.npmjs.com/package/yargs
- Biblioteca commander para crear interfaces de línea de comandos: https://www.npmjs.com/package/commander
