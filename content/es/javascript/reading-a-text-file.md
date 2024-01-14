---
title:                "Javascript: Leyendo un archivo de texto"
programming_language: "Javascript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Por qué leer un archivo de texto en Javascript?

Si estás buscando aprender a programar en Javascript, es importante que sepas cómo leer y manipular archivos de texto. Esto te permitirá trabajar con datos externos, como por ejemplo leer un archivo de configuración o procesar datos de un formulario. En esta entrada, te enseñaremos los conceptos básicos de cómo leer un archivo de texto en Javascript, para que puedas aplicar esta habilidad en tus proyectos.

## Cómo hacerlo



Para leer un archivo de texto en Javascript, primero debes entender cómo funciona el sistema de archivos. En la mayoría de los casos, esto se maneja a través de la función `require()`, que carga módulos de Node.js en tu programa. Entonces, para acceder a un archivo de texto, debes instanciar un objeto de tipo `fs`, que es el módulo de Node.js para trabajar con archivos.

Una vez que tienes instanciado el objeto `fs`, puedes utilizar su método `readFileSync()` para leer el contenido de un archivo de texto. Este método toma como parámetro la ruta del archivo que quieres leer y devuelve el contenido del archivo en forma de cadena.

Veamos un ejemplo de cómo leer un archivo de texto y mostrar su contenido en consola:

```
```Javascript
// Cargamos módulo fs
const fs = require('fs');

// Leemos el archivo de texto
const contenido = fs.readFileSync('archivo.txt', 'utf-8');

// Mostramos el contenido en consola
console.log(contenido);
```

Si el contenido del archivo de texto es "¡Hola, mundo!", al ejecutar este código, aparecerá en consola la siguiente salida:

```
¡Hola, mundo!
```

## Profundizando

Además de `readFileSync()`, el objeto `fs` también cuenta con otros métodos para trabajar con archivos de texto, como `readFile()` y `writeFile()`. En el caso de `readFile()`, este método permite leer el contenido de un archivo de manera asíncrona, lo que significa que no bloquea la ejecución de otros códigos mientras se lee el archivo. 

También es importante tener en cuenta que, al trabajar con archivos de texto, es necesario manejar errores. Por ejemplo, si el archivo que estás intentando leer no existe, tu programa deberá manejar esa situación y mostrar un mensaje adecuado al usuario.

En resumen, aprender a leer archivos de texto en Javascript te permitirá manipular datos externos y aplicar esta habilidad en tus proyectos de programación.

## Ver también

Aquí te dejamos algunos enlaces para aprender más sobre cómo leer archivos de texto en Javascript:

- [Documentación oficial de Node.js sobre el módulo fs](https://nodejs.org/api/fs.html)
- [Video tutorial sobre cómo leer y escribir archivos con fs en Node.js](https://www.youtube.com/watch?v=OZGm3UURUoY)
- [Blog post explicando en detalle cómo funciona el sistema de archivos en Node.js](https://www.digitalocean.com/community/tutorials/node-js-file-systems)

Esperamos que esta entrada te haya sido útil y que puedas poner en práctica tus conocimientos en tus proyectos de Javascript. ¡Hasta la próxima!