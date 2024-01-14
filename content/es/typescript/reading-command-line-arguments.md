---
title:                "TypeScript: Leyendo argumentos de línea de comando"
simple_title:         "Leyendo argumentos de línea de comando"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez has tenido la necesidad de pasar argumentos por línea de comando en un programa TypeScript? Tal vez estás construyendo una aplicación de línea de comandos o simplemente quieres tener un poco más de control sobre cómo se ejecuta tu aplicación. Sea cual sea el caso, leer argumentos por línea de comando es una habilidad útil que puede facilitar tu trabajo.

## Cómo hacerlo

Para leer argumentos por línea de comando en TypeScript, podemos utilizar la variable `process` y su propiedad `argv`. Esta propiedad contiene un array con todos los argumentos pasados en la línea de comando, incluyendo el nombre del archivo que se está ejecutando. Veamos un ejemplo de cómo podemos acceder a estos argumentos y mostrarlos por consola:

```typescript
const args = process.argv;
console.log(args);
```

Si ejecutamos este código con el comando `ts-node index.ts arg1 arg2 arg3`, obtendremos la siguiente salida por consola:

```shell
[
  'C:\\ruta\\hacia\\typescript.node',
  'C:\\ruta\\hacia\\index.ts',
  'arg1',
  'arg2',
  'arg3'
]
```

Como podemos ver, la variable `process.argv` contiene todos los argumentos que pasamos por línea de comando, incluyendo el nombre del archivo que se está ejecutando y la ruta al ejecutable de TypeScript.

Pero, ¿qué sucede si queremos acceder a argumentos específicos? Podemos hacerlo utilizando el índice del array. Por ejemplo, si queremos mostrar solo el segundo argumento, podemos escribir `console.log(args[2])` y obtendremos `arg1` como resultado.

Podemos incluso utilizar la librería `yargs` para facilitar la lectura de argumentos por línea de comando. Veamos un ejemplo de cómo podríamos utilizar `yargs` para acceder a los argumentos pasados al ejecutar una aplicación:

```typescript
import yargs from 'yargs';

const argv = yargs(process.argv).argv;
const username = argv.username;

console.log(`Hola ${username}, bienvenido a mi aplicación.`);
```

Si ejecutamos este código con el comando `ts-node index.ts --username John`, obtendremos la siguiente salida por consola:

```shell
Hola John, bienvenido a mi aplicación.
```

## Profundizando un poco más

Ahora que sabemos cómo leer argumentos por línea de comando en TypeScript, es importante mencionar que también podemos pasar argumentos opcionales utilizando la librería `yargs`. Podemos definir argumentos opcionales utilizando `.option()` y acceder a ellos utilizando el método `check()` de `argv`. Por ejemplo, si queremos hacer que el argumento `--greeting` sea opcional, podemos escribir lo siguiente:

```typescript
import yargs from 'yargs';

const argv = yargs(process.argv)
  .option('greeting', {
    alias: 'g',
    type: 'string',
    describe: 'Mensaje de bienvenida'
  })
  .check(argv => {
    if (!argv.greeting) {
      argv.greeting = 'Hola';
    }

    return true;
  })
  .argv;

console.log(`${argv.greeting}, bienvenido a mi aplicación.`);
```

Si ejecutamos este código con el comando `ts-node index.ts` obtendremos la siguiente salida por consola:

```shell
Hola, bienvenido a mi aplicación.
```

Pero si ejecutamos el mismo código con el comando `ts-node index.ts --greeting Welcome`, obtendremos lo siguiente:

```shell
Welcome, bienvenido a mi aplicación.
```

## Ver también

- [Documentación de process.argv en la documentación oficial de Node.js](https://nodejs.org/dist/latest-v16.x/docs/api/process.html#process_process_argv)
- [Documentación de yargs en su página oficial](https://www.npmjs.com/package/yargs)