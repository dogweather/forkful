---
title:    "TypeScript: Escribiendo en el error estándar"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Por qué 

Escribir a la salida estándar de error puede resultar útil para quienes deseen depurar su código TypeScript o mostrar mensajes de error personalizados en sus aplicaciones. Esta práctica permite imprimir información adicional que puede ser útil para encontrar y corregir errores en el código.

## Cómo hacerlo

Para escribir a la salida estándar de error en TypeScript, podemos utilizar el método `console.error()`. Este método acepta un argumento que puede ser una cadena de texto o un objeto, y lo imprimirá en la consola de error.

```TypeScript
let num: number = 10;
console.error("El valor de la variable num es " + num);
```

La salida de este código será:

```
El valor de la variable num es 10
```

También podemos utilizar plantillas de cadena para imprimir información adicional de forma más dinámica. Por ejemplo:

```TypeScript
let nombre: string = "María";
let edad: number = 25;
console.error(`El nombre de la usuaria es ${nombre} y tiene ${edad} años.`);
```

La salida será:

```
El nombre de la usuaria es María y tiene 25 años.
```

## Profundizando

Además de utilizar el método `console.error()`, también podemos acceder a la salida estándar de error a través del objeto `process.stderr`. Esto puede ser útil en ciertos casos, como en la creación de un proceso hijo que envíe mensajes de error al proceso padre.

El objeto `process.stderr` es un flujo de escritura, por lo que podemos utilizar el método `write()` para enviar datos a la salida estándar de error. Por ejemplo:

```TypeScript
import * as fs from "fs";

// Creamos un archivo utilizando el método writeFile de la librería fs
fs.writeFile("datos.txt", "Información importante", (error) => {
  if (error) throw error;
  // Si se produce un error, lo imprimimos a la salida estándar de error
  process.stderr.write("Ha habido un error en la creación del archivo");
});
```

## Ver también

- [Console API en TypeScript](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2.html#console-api)
- [Manipulando ficheros con fs en TypeScript](https://www.digitalocean.com/community/tutorials/nodejs-creando-y-leyendo-archivos-con-fs-es)