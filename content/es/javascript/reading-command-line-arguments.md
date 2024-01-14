---
title:                "Javascript: Leyendo argumentos de línea de comando"
programming_language: "Javascript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## ¿Por qué leer argumentos de línea de comandos?

Si eres un programador de Javascript, seguramente te hayas encontrado con la necesidad de leer argumentos de línea de comandos en tus proyectos. Ya sea para crear una aplicación en línea de comandos o para tener una funcionalidad adicional en tu aplicación web, leer argumentos de línea de comandos puede ser una habilidad muy útil en tu caja de herramientas de programación. En esta publicación, exploraremos cómo leer argumentos de línea de comandos en Javascript y por qué es importante saber cómo hacerlo.

## Cómo hacerlo

La forma más común de leer argumentos de línea de comandos en Javascript es utilizando el objeto `process` que viene incorporado en el entorno de Node.js. Este objeto tiene una propiedad llamada `argv` que contiene una lista de todos los argumentos de línea de comandos ingresados al ejecutar tu script.

Veamos un ejemplo de cómo leer los argumentos de línea de comandos y mostrarlos por consola:

```javascript
// Crear una variable para almacenar los argumentos
let args = process.argv;

// Iterar a través de los argumentos y mostrarlos por consola
for (let i = 0; i < args.length; i++) {
  console.log("Argumento #" + i + ": " + args[i]);
}
```

Si ejecutamos este script con el comando `node index.js uno dos tres`, el mensaje que veremos por consola será:

```
Argumento #0: node
Argumento #1: /home/usuario/index.js
Argumento #2: uno
Argumento #3: dos
Argumento #4: tres
```

Como puedes ver, el primer argumento en la lista es siempre el ejecutable utilizado para correr el script, seguido por la ruta del archivo y luego los argumentos ingresados por el usuario.

También podemos utilizar el método `slice()` en el objeto `process.argv` para obtener solamente los argumentos ingresados por el usuario, ignorando el ejecutable y la ruta del archivo.

```javascript
let args = process.argv.slice(2); // Se omite el primer y segundo elemento (ejecutable y ruta del archivo)
console.log(args);
```

Si volvemos a ejecutar el script con los mismos argumentos, el mensaje por consola será:

```
["uno", "dos", "tres"]
```

De esta forma, podemos acceder a los argumentos ingresados por el usuario dentro de nuestro código de Javascript y utilizarlos para diferentes propósitos.

## Profundizando

Ahora que sabemos cómo leer los argumentos de línea de comandos en Javascript, es importante profundizar un poco más en el tema. Por ejemplo, ¿qué pasa si queremos que nuestro código sea más robusto y maneje diferentes escenarios? ¿Qué pasa si queremos validar los argumentos o convertirlos a diferentes tipos de datos?

Para manejar estas situaciones, es importante tener en cuenta que los argumentos siempre se reciben como cadenas de caracteres, por lo que pueden requerir ciertas transformaciones dependiendo de lo que queramos hacer con ellos. Por ejemplo, si queremos que un argumento sea un número entero, debemos convertirlo utilizando `parseInt()` o `Number()`. También podemos utilizar condicionales y expresiones regulares para validar los argumentos y mostrar mensajes de error en caso de que no cumplan con ciertas condiciones.

Otra técnica útil es utilizar módulos de terceros, como `yargs` o `commander`, que hacen que el proceso de leer argumentos de línea de comandos sea más sencillo y robusto, permitiéndonos incluso definir opciones y argumentos específicos para nuestro script.

## Ver también

- [Documentación de Node.js sobre el objeto Process](https://nodejs.org/api/process.html)
- [Módulo yargs en NPM](https://www.npmjs.com/package/yargs)
- [Módulo commander en NPM](https://www.npmjs.com/package/commander)