---
title:    "TypeScript: Leyendo argumentos de línea de comando"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Por qué

Leer argumentos de línea de comandos puede parecer una tarea aburrida y poco útil, pero en realidad es una habilidad muy importante para cualquier programador. Aprender a leer y utilizar los argumentos de línea de comandos puede mejorar la eficiencia y la flexibilidad de tu código, ya que proporciona una forma de interactuar con el programa a través de la entrada del usuario.

## Cómo hacerlo

En TypeScript, podemos utilizar el objeto `process` para acceder a los argumentos de línea de comandos. Este objeto contiene una propiedad llamada `argv` que es un arreglo que contiene todos los argumentos pasados en la línea de comandos. Veamos un ejemplo de cómo podemos leer y utilizar estos argumentos:

```TypeScript
console.log(process.argv); // Imprime todos los argumentos
console.log(process.argv[2]); // Imprime el tercer argumento
```

Si ejecutamos nuestro código con el siguiente comando: `node app.js argumento1 argumento2 argumento3`, el resultado sería:

```TypeScript
["node", "app.js", "argumento1", "argumento2", "argumento3"]
"argumento1"
```

Ahora podemos utilizar estos argumentos en nuestro código como lo haríamos con cualquier otra variable.

## Profundizando

En nuestro ejemplo anterior, podemos notar que el primer elemento de `process.argv` siempre es el nombre del programa que se está ejecutando, mientras que el segundo elemento es el nombre del archivo TypeScript que estamos ejecutando. A partir del tercer elemento, podemos acceder a los argumentos pasados por el usuario.

También es importante tener en cuenta que los argumentos de línea de comandos se pasan como cadenas de texto, por lo que es posible que necesitemos convertirlos a otros tipos de datos según sea necesario. Por ejemplo, si nuestro programa espera un número entero como argumento, podemos usar la función `parseInt()` para convertir el argumento a un número.

## Ver también

- [Documentación de process en la documentación oficial de TypeScript](https://www.typescriptlang.org/docs/handbook/integrating-with-build-tools.html#integrating-with-build-tools)
- [Artículo sobre el objeto process en el blog de Node.js](https://nodejs.dev/the-process-object)