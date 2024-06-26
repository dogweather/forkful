---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:28.365833-07:00
description: "C\xF3mo hacerlo: TypeScript por s\xED mismo no maneja directamente las\
  \ operaciones de archivos ya que se compila a JavaScript, que tradicionalmente se\
  \ ejecuta\u2026"
lastmod: '2024-03-13T22:44:58.821465-06:00'
model: gpt-4-0125-preview
summary: "TypeScript por s\xED mismo no maneja directamente las operaciones de archivos\
  \ ya que se compila a JavaScript, que tradicionalmente se ejecuta en el navegador\
  \ con acceso limitado al sistema de archivos."
title: Escribiendo un archivo de texto
weight: 24
---

## Cómo hacerlo:
TypeScript por sí mismo no maneja directamente las operaciones de archivos ya que se compila a JavaScript, que tradicionalmente se ejecuta en el navegador con acceso limitado al sistema de archivos. Sin embargo, cuando se usa en un entorno Node.js, el módulo `fs` (Sistema de Archivos) proporciona funcionalidad para escribir archivos.

### Usando el módulo fs de Node.js
Primero, asegúrate de estar trabajando en un entorno Node.js. Luego, usa el módulo `fs` para escribir archivos de texto. Aquí hay un ejemplo básico:

```typescript
import * as fs from 'fs';

const data = '¡Hola, mundo!';
const filePath = './message.txt';

fs.writeFile(filePath, data, 'utf8', (err) => {
    if (err) throw err;
    console.log('¡El archivo ha sido guardado!');
});
```

Esto escribirá de manera asíncrona "¡Hola, mundo!" en `message.txt`. Si el archivo no existe, Node.js lo crea; si existe, Node.js lo sobrescribe.

Para escribir archivos de manera síncrona, usa `writeFileSync`:

```typescript
import * as fs from 'fs';

const data = '¡Hola de nuevo, mundo!';
const filePath = './message.txt';

try {
    fs.writeFileSync(filePath, data, 'utf8');
    console.log('¡El archivo ha sido guardado!');
} catch (err) {
    console.error(err);
}
```

### Usando bibliotecas de terceros populares
Aunque el módulo `fs` nativo es poderoso, algunos desarrolladores prefieren usar bibliotecas de terceros por conveniencia y funcionalidad adicional. `fs-extra` es una opción popular que extiende a `fs` y hace las operaciones de archivo más sencillas.

Primero, necesitarás instalar `fs-extra`:

```
npm install fs-extra
```

Luego, puedes usarlo en tu archivo TypeScript para escribir contenido de texto:

```typescript
import * as fs from 'fs-extra';

const data = '¡Esto es fs-extra!';
const filePath = './extraMessage.txt';

// Usando async/await
async function writeFile() {
    try {
        await fs.writeFile(filePath, data, 'utf8');
        console.log('¡El archivo ha sido guardado con fs-extra!');
    } catch (err) {
        console.error(err);
    }
}

writeFile();
```

Este fragmento de código hace lo mismo que los ejemplos anteriores de `fs`, pero utiliza la biblioteca `fs-extra`, ofreciendo una sintaxis más clara para manejar promesas.
