---
title:                "TypeScript: Leyendo argumentos de línea de comando"
programming_language: "TypeScript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por qué leer argumentos de línea de comandos en TypeScript

Si eres un programador de TypeScript, es muy probable que hayas escuchado sobre la lectura de argumentos de línea de comandos en tus programas. Esta técnica puede ser útil para interactuar con tu programa de una manera más dinámica y personalizada. En este artículo, te explicaremos por qué es importante aprender a leer argumentos de línea de comandos en TypeScript y cómo hacerlo.

## Cómo hacerlo

La lectura de argumentos de línea de comandos en TypeScript es un proceso relativamente sencillo. Primero, necesitas importar el módulo `process` en tu programa. Luego, puedes acceder a los argumentos ingresados por el usuario a través de la propiedad `argv` del objeto `process`.

Veamos un ejemplo de cómo leer y mostrar los argumentos de línea de comandos en TypeScript:

```TypeScript
import process from 'process';

const args = process.argv;

// Los primeros dos elementos de args son el directorio de Node.js y el archivo TypeScript.
// Los siguientes elementos son los argumentos ingresados por el usuario.
for (let i = 2; i < args.length; i++) {
  console.log(`Argumento ${i - 1}: ${args[i]}`);
}

```

Si ejecutamos este programa con el siguiente comando:

```
node programa.ts hola mundo
```

El resultado sería:

```
Argumento 1: hola
Argumento 2: mundo
```

Puedes ver que los argumentos ingresados por el usuario son accesibles a través de un array en el módulo `process`.

## Profundizando

Ahora que sabes cómo leer argumentos de línea de comandos en TypeScript, puede ser útil saber más sobre cómo funciona este proceso en detalle. Los argumentos de línea de comandos son cadenas de texto que se pasan a tu programa al momento de ejecutarlo. Pueden ser útiles para proporcionar información como opciones, rutas de archivos o cualquier otro tipo de dato que tu programa necesite para funcionar correctamente.

Además de la propiedad `argv`, el objeto `process` también incluye otras propiedades y métodos útiles para interactuar con el entorno de ejecución de tu programa. También puedes utilizar la librería `yargs` para facilitar la lectura y manejo de argumentos de línea de comandos en tu código.

¡Explora y experimenta con la lectura de argumentos de línea de comandos en TypeScript para descubrir sus beneficios y posibilidades!

## Ver también

- [Documentación oficial de Node.js sobre el módulo process](https://nodejs.org/api/process.html)
- [Librería "yargs" para manejo de argumentos de línea de comandos en TypeScript](https://www.npmjs.com/package/yargs)