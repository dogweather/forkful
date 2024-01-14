---
title:    "TypeScript: Imprimiendo salidas de depuración"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Por qué
La impresión de salida de depuración es una herramienta útil para los desarrolladores, ya que les permite ver y comprender el estado interno de su código mientras se está ejecutando. También es útil para detectar errores y solucionar problemas en el código.

## Cómo hacerlo
Para imprimir salida de depuración en TypeScript, puedes utilizar la función "console.log ()". Por ejemplo:

```TypeScript
let nombre = "María";
console.log("Hola", nombre);
```

Esto imprimirá "Hola María" en la consola, lo que te permitirá ver el valor de la variable "nombre". Puedes imprimir cualquier tipo de dato en la consola, incluyendo strings, números, objetos y arrays.

La salida de depuración también se puede utilizar para verificar si ciertas secciones de código se están ejecutando o para seguir el flujo de ejecución. Por ejemplo:

```TypeScript
console.log("Iniciando bucle for");
for (let i = 0; i < 5; i++){
  console.log(i);
}
console.log("Fin de bucle for");
```

La salida en la consola sería:

```
Iniciando bucle for
0
1
2
3
4
Fin de bucle for
```

## Profundizando
Además de la función "console.log ()", TypeScript también ofrece otras opciones para imprimir salida de depuración. Puedes utilizar "console.debug ()" para imprimir mensajes de depuración más detallados, "console.error ()" para imprimir mensajes de error y "console.warn ()" para imprimir advertencias.

También puedes utilizar plantillas de cadena para imprimir variables y mensajes de una manera más eficiente y legible. Por ejemplo:

```TypeScript
let nombre = "Juan";
console.log(`Hola ${nombre}, ¿cómo estás?`);
```

Esto imprimiría "Hola Juan, ¿cómo estás?" en la consola.

## Ver también
- [Documentación de TypeScript sobre Debugging](https://www.typescriptlang.org/docs/handbook/debugging.html)
- [Tutorial de Debugging en TypeScript](https://dev.to/omiusone/debugging-typescript-using-vscode-4nfc)
- [Vídeo de Debugging con TypeScript](https://www.youtube.com/watch?v=qjPqIP_LmYM)