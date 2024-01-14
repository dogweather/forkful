---
title:                "TypeScript: Leyendo un archivo de texto"
programming_language: "TypeScript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué leer un archivo de texto en TypeScript

Si estás aprendiendo TypeScript o simplemente quieres mejorar tus habilidades de programación, leer un archivo de texto es una práctica común y útil. Te permite manipular y analizar datos de manera eficiente, lo que es especialmente importante en proyectos de gran escala.

## Cómo leer un archivo de texto en TypeScript

Para leer un archivo de texto en TypeScript, primero debemos importar la biblioteca `fs` (file system). Luego, podemos utilizar la función `readFileSync` para cargar el archivo en una variable y la función `toString` para convertirlo en una cadena de texto. A continuación, podemos imprimir el contenido del archivo en la consola como se muestra a continuación:

```TypeScript
import * as fs from 'fs';
const file = fs.readFileSync('ruta_al_archivo.txt').toString();
console.log(file);
```

El resultado será el contenido del archivo de texto impreso en la consola.

## Profundizando en la lectura de archivos de texto

Ahora que sabemos cómo leer un archivo de texto en TypeScript, podemos profundizar un poco más. Podemos utilizar el método `split` para dividir la cadena de texto en líneas y el método `forEach` para recorrer cada línea e imprimir su contenido en la consola. También podemos utilizar expresiones regulares para filtrar o buscar información específica dentro del archivo.

```TypeScript
import * as fs from 'fs';
const file = fs.readFileSync('ruta_al_archivo.txt').toString();
const lines = file.split('\n');
lines.forEach(line => {
  console.log(line);
})
```

Incluso podemos utilizar bibliotecas externas, como `csv-parser` o `xlsx`, para leer y manipular diferentes tipos de archivos de texto de manera más eficiente.

## Ver también

- [Node.js documentación de fs](https://nodejs.org/docs/latest-v14.x/api/fs.html)
- [Tutorial de TypeScript en español](https://typescript-es.com/)