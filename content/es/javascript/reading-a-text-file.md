---
title:    "Javascript: Leyendo un archivo de texto"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué

Para muchos programadores, leer un archivo de texto puede parecer una tarea sencilla y aburrida, pero en realidad es una habilidad valiosa que puede ahorrar tiempo y mejorar la eficiencia en el desarrollo de software. Leer un archivo de texto permite acceder a información almacenada externamente, como datos de configuración o texto para un programa, lo que lo convierte en una herramienta esencial para cualquier desarrollador.

## Cómo hacerlo

Para leer un archivo de texto en Javascript, se utiliza la función `readFileSync` del módulo `fs`. Esta función toma dos argumentos: el nombre del archivo que se va a leer y el formato en el que se desea leerlo. Por ejemplo:

```Javascript
const fs = require('fs');

// Leer archivo de texto en formato UTF-8
const texto = fs.readFileSync('miarchivo.txt', 'utf-8');
console.log(texto);
```

En este ejemplo, utilizamos el módulo `fs` y su método `readFileSync` para leer el archivo `miarchivo.txt` en formato UTF-8. Luego, imprimimos el contenido del archivo en la consola. Es importante tener en cuenta que si el archivo no se encuentra en la misma carpeta que el archivo de Javascript, se debe proporcionar la ruta completa del archivo.

## Profundizando

Además de la función `readFileSync`, también existe la función `readFile` del mismo módulo. La diferencia entre ambas es que `readFile` es asíncrona, lo que significa que no bloqueará la ejecución del programa mientras se lee el archivo. Esto puede ser útil para archivos más grandes, pero requiere el uso de callbacks o promesas para manejar el resultado de la operación.

Además, es importante recordar que al leer un archivo de texto, se obtiene una cadena de texto que luego se puede manipular y utilizar en el programa. Es posible, por ejemplo, utilizar la función `split` para dividir la cadena en diferentes partes si el archivo contiene información estructurada.

## Ver también

- Documentación de Node.js sobre el módulo `fs`: https://nodejs.org/api/fs.html
- Ejemplos de uso de `readFileSync`: https://www.geeksforgeeks.org/node-js-fs-readfilesync-method/
- Uso de `readFile` y callbacks: https://www.digitalocean.com/community/tutorials/nodejs-reading-files