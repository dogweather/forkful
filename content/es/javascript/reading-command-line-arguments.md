---
title:                "Leyendo argumentos de línea de comandos"
html_title:           "Javascript: Leyendo argumentos de línea de comandos"
simple_title:         "Leyendo argumentos de línea de comandos"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por qué

Si eres un desarrollador de Javascript, es muy probable que hayas escuchado sobre los argumentos de línea de comandos. Pero, ¿por qué deberías dedicarle tiempo a aprender a usarlos? La respuesta es simple: los argumentos de línea de comandos son una herramienta poderosa que te permitirá interactuar con tus programas de una manera más eficiente.

## Cómo

Para leer los argumentos de línea de comandos en Javascript, puedes utilizar el objeto `process.argv`. Este objeto contiene un array con todos los argumentos pasados al programa en forma de cadenas de texto. Veamos un ejemplo:

```Javascript
// programa.js
console.log(process.argv);
```
Si ejecutamos este programa desde la línea de comandos con `node programa.js argumento1 argumento2`, veremos el siguiente resultado:

```
[ 'node',
  '/ruta/al/archivo/programa.js',
  'argumento1',
  'argumento2' ]
```

Como puedes ver, el primer elemento del array es siempre la ruta del ejecutable utilizado para correr el programa. Los argumentos pasados a continuación se presentan en orden.

También es posible acceder a cada argumento de manera individual utilizando su índice en el array. Por ejemplo, `process.argv[2]` sería igual a `'argumento1'`. Ten en cuenta que todos los argumentos son tratados como cadenas de texto, por lo que si necesitas utilizarlos como otro tipo de dato, deberás convertirlos manualmente.

## Deep Dive

El módulo `process` de NodeJS también proporciona otros métodos para trabajar con los argumentos de línea de comandos. Algunos de ellos son `process.argv0`, que contiene el valor de `argv[0]`, y `process.execArgv`, que contiene los argumentos pasados al ejecutable de Node antes de la siguiente versión del script. También puedes utilizar `process.exit()` para salir del programa y especificar un código de salida.

Para profundizar aún más en el manejo de argumentos de línea de comandos, consulta la documentación oficial para obtener más detalles y ejemplos prácticos.

## Ver también

- [Documentación oficial de `process.argv`](https://nodejs.org/api/process.html#process_process_argv)
- [Tutorial de Codecademy sobre argumentos de línea de comandos en NodeJS](https://www.codecademy.com/learn/rpc-node-api/modules/rpc-node-api/cheatsheet)
- [Ejemplos prácticos de uso de `process.argv`](https://flaviocopes.com/node-cli-args/)