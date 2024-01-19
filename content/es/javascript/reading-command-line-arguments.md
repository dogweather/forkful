---
title:                "Leyendo argumentos de la línea de comandos"
html_title:           "Bash: Leyendo argumentos de la línea de comandos"
simple_title:         "Leyendo argumentos de la línea de comandos"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Leer argumentos desde la línea de comandos es leer los datos que el usuario introduce después del nombre del programa. Los programadores lo hacen para permitir a los usuarios influir en cómo se ejecuta el programa.

## Cómo se hace:

Aquí tiene un ejemplo simple. Usaremos el objeto process de Node.js y su propiedad argv, que es un array. Los primeros dos elementos del array son 'node' y el nombre del archivo. Los argumentos de la línea de comandos comienzan desde el tercer elemento.

```Javascript
process.argv.forEach((value, index) => {
  console.log(`${index}: ${value}`);
});
```

Si ejecutamos el programa con `node programa.js uno dos tres`, la salida será:

```Javascript
0: node
1: /ruta/al/archivo/programa.js
2: uno
3: dos
4: tres
```

## Análisis Profundo:

Históricamente, en C y otros lenguajes similares, los argumentos de línea de comandos eran pasados al programa a través de los argumentos de la funcion main. Node.js mantuvo este patrón con process.argv.

Existen alternativas para leer argumentos de la línea de comandos más complejos. Libraries como 'yargs' o 'commander' pueden manejar argumentos con banderas y subcomandos, por ejemplo,`myprogram -d --force file.txt`.

Por defecto, process.argv devuelve todo como strings. Si necesita tipos distintos, tendrá que convertir los datos manualmente.

## Vea También:

- Documentación de Node.js process.argv: https://nodejs.org/docs/latest/api/process.html#process_process_argv
- Comparativa de libraries para argumentos de línea de comandos: https://www.npmtrends.com/commander-vs-minimist-vs-yargs
- Tutorial de argumentos de línea de comandos en Node.js: https://flaviocopes.com/node-command-line-args/