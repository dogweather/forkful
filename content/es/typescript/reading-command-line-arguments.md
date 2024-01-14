---
title:    "TypeScript: Leyendo argumentos de línea de comando"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por qué leer argumentos por línea de comando en TypeScript

La lectura de argumentos por línea de comando en TypeScript puede ser una herramienta valiosa para interactuar con nuestros programas de forma más dinámica. Por ejemplo, podemos permitir que el usuario especifique opciones o parámetros al ejecutar nuestro código en lugar de tenerlos escritos de antemano. Esto les da a los usuarios más control y flexibilidad en la ejecución de nuestros programas.

## Cómo hacerlo

En TypeScript, podemos acceder a los argumentos de línea de comando utilizando el objeto `process`. Primero, importamos este objeto en nuestro código:

```TypeScript
import process from 'process';
```

Luego, podemos usar el arreglo `process.argv` para acceder a todos los argumentos pasados al programa en la línea de comando. Por ejemplo, si queremos acceder al segundo argumento, podemos hacerlo de la siguiente manera:

```TypeScript
process.argv[2];
```

También podemos usar el método `slice()` para obtener un subconjunto de los argumentos. Por ejemplo, si queremos obtener todos los argumentos pasados después del tercero, podemos hacerlo de la siguiente manera:

```TypeScript
process.argv.slice(3);
```

Finalmente, podemos imprimir los argumentos en la consola para ver cómo se ven. Por ejemplo, si llamamos a nuestro programa con los argumentos `node index.ts hola mundo`, el siguiente código:

```TypeScript
console.log(process.argv);
```

Imprimirá lo siguiente en la consola:

```
[ 'node', 'index.ts', 'hola', 'mundo' ]
```

## Profundizando

Además de acceder a los argumentos de línea de comando, también podemos usar módulos externos en TypeScript para analizarlos de forma más compleja. Por ejemplo, podemos usar el módulo `yargs` para crear opciones y parámetros con alias y descripciones, y luego acceder a ellos fácilmente en nuestro código.

Para instalar `yargs`, usamos el siguiente comando en nuestra terminal:

```
npm install yargs
```

Luego, en nuestro código TypeScript, importamos `yargs` y configuramos nuestros argumentos utilizando el método `command()`:

```TypeScript
import yargs from 'yargs';

yargs.command('saludar [nombre]', 'Imprime un saludo personalizado', {
  nombre: {
    alias: 'n',
    describe: 'Nombre de la persona a saludar',
    type: 'string'
  }
});
```

Finalmente, podemos usar el objeto `argv` de `yargs` para acceder a los argumentos pasados en la línea de comando y realizar acciones en consecuencia. Por ejemplo, si llamamos a nuestro programa con el comando `node index.ts saludar -n Juan`, podremos acceder al nombre `Juan` utilizando `argv.n`.

## Ver también

Aquí hay algunos recursos adicionales que pueden ser útiles al trabajar con argumentos de línea de comando en TypeScript:

- [Documentación oficial de TypeScript sobre argumentos de línea de comando](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-4-0.html#labeled-tuple-elements)
- [Documentación oficial de `yargs`](https://github.com/yargs/yargs)
- [Tutorial de Command-Line Applications en TypeScript](https://brianflove.com/2019/12/02/typescript-command-line-args/)