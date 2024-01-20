---
title:                "Leyendo argumentos de la línea de comandos"
html_title:           "Bash: Leyendo argumentos de la línea de comandos"
simple_title:         "Leyendo argumentos de la línea de comandos"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?

Leer argumentos de lineas de comando permite a los programas recibir datos desde la terminal directamente. Los programadores lo usan para personalizar comportamientos de programas y pruebas, así como para facilitar la interacción entre programas.

## Cómo hacerlo:

Lamentablemente, la arquitectura actual de Elm (0.19.1) no soporta la lectura de argumentos de la línea de comando directamente. Elm se enfoca principalmente en el desarrollo de aplicaciones web en lugar de aplicaciones de consola. Sin embargo, para fines de ilustración, aquí hay un ejemplo de cómo se haría en un lenguaje que lo permita, como Node.js :

```Node.js
// Proceso para obtener argumentos de la linea de comando
process.argv.forEach((valor, index) => {
    console.log(`${index}: ${valor}`);
});
```

Si se ejecuta este programa con el comando `node program.js uno dos tres`, la salida será:
```
0: node
1: /ruta/a/program.js
2: uno
3: dos
4: tres
```

## Inmersión profunda:

Aunque Elm no soporta la lectura de argumentos de la línea de comando, siempre se puede usar junto con lenguajes que sí lo hacen, como JavaScript. Esta limitación es una decisión de diseño para mantener la simplicidad y seguridad de Elm. Los argumentos de la línea de comando suelen usarse en scripts y aplicaciones de la consola, mientras que Elm está diseñado para aplicaciones web.

Históricamente, muchos lenguajes de programación permiten leer argumentos de línea de comando para la interacción de programas. En Elm, esta interactividad se maneja principalmente en el front end a través de funciones de entrada y salida.

## Ver También:

- [Documentación Oficial de Elm](https://elm-lang.org/docs)
- [Uso de argumentos de línea de comando en Node.js](https://nodejs.dev/learn/nodejs-accept-arguments-from-the-command-line) 
- [Parámetros y argumentos de la línea de comandos en bash](https://www.gnu.org/software/bash/manual/)