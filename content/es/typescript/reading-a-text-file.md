---
title:    "TypeScript: Leyendo un archivo de texto"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Por qué

Lidiar con la lectura de archivos de texto puede ser una tarea tediosa y propensa a errores cuando se realiza en otros lenguajes de programación. Afortunadamente, TypeScript ofrece una solución simple y eficiente para leer archivos de texto, lo que puede ahorrar tiempo y aumentar la precisión en su código.

## Cómo hacerlo

Para leer un archivo de texto en TypeScript, primero necesitamos importar el módulo `fs`, que es el módulo de sistema de archivos incorporado en Node.js. Una vez importado, podemos utilizar la función `readFile` para leer el contenido de nuestro archivo de texto.

```TypeScript
import * as fs from 'fs';

fs.readFile('archivo.txt', 'utf8', (err, data) => {
  if (err) {
    console.error(err);
  } else {
    console.log(data);
  }
});
```

En este ejemplo, utilizamos la función `readFile` para leer un archivo llamado "archivo.txt" en formato UTF-8. La función también acepta un tercer parámetro opcional, que puede ser una función de devolución de llamada para manejar cualquier error que pueda ocurrir durante la lectura del archivo.

Una vez que se completa la lectura del archivo, el contenido se almacenará en la variable `data`. Podemos usar esta variable para realizar cualquier operación adicional que necesitemos con el contenido del archivo.

## Profundizando

Además de la función `readFile`, el módulo `fs` también ofrece otras opciones para leer archivos de texto en TypeScript. Algunas de estas opciones incluyen `readFileSync`, que lee el archivo de forma sincrónica y devuelve el contenido directamente, y `createReadStream`, que crea un flujo de lectura para archivos grandes.

También es importante tener en cuenta que a veces el contenido del archivo puede tener un formato que no sea UTF-8, como ASCII. En estos casos, necesitaremos especificar el formato correcto al usar la función `readFile`.

## Ver también

- [Documentación de Node.js sobre el módulo fs](https://nodejs.org/api/fs.html)
- [Tutorial de TypeScript sobre la lectura de archivos](https://www.typescriptlang.org/docs/handbook/working-with-files.html)