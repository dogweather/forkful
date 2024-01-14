---
title:                "TypeScript: Imprimiendo salida de depuración"
simple_title:         "Imprimiendo salida de depuración"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por qué imprimir salidas de depuración es importante

La impresión de salidas de depuración es una técnica esencial para cualquier programador. Proporciona una forma de inspeccionar el estado de su código durante la ejecución y ayuda a identificar posibles errores o problemas. Sin embargo, es importante saber cuándo y cómo utilizar esta técnica para obtener resultados efectivos.

## Cómo hacerlo

Para imprimir salidas de depuración en TypeScript, puede utilizar la función `console.log()`. Esta función toma uno o más argumentos y los imprime en la consola del navegador o en la terminal de su editor de texto. Por ejemplo:

```TypeScript
let mensaje = "¡Hola, mundo!";
console.log(mensaje);
// Salida: ¡Hola, mundo!
```

También puede utilizar la interpolación de cadenas para imprimir variables en un mensaje. En este caso, debe utilizar el carácter de dólar seguido de llaves `{}` para indicar dónde se debe insertar el valor de la variable. Por ejemplo:

```TypeScript
let edad = 25;
console.log(`Tengo ${edad} años.`);
// Salida: Tengo 25 años.
```

Otra forma útil de imprimir salidas de depuración es utilizando la función `console.dir()`. Esta función toma un objeto como argumento y lo imprime en formato de árbol para una mejor visualización. Por ejemplo:

```TypeScript
let persona = {
  nombre: "María",
  edad: 30,
  profesión: "Desarrolladora"
};
console.dir(persona);
// Salida:
// { nombre: "María", edad: 30, profesión: "Desarrolladora" }
```

## Profundizando

Cuando utilice la función `console.log()`, también puede utilizar etiquetas de tiempo y estilos para personalizar su salida. Puede utilizar `console.time()` y `console.timeEnd()` para medir el tiempo de ejecución de una sección específica de su código. Por ejemplo:

```TypeScript
console.time("Ejecución de bucle");
for (let i = 0; i < 100000; i++) {
  // Hacer algo
}
console.timeEnd("Ejecución de bucle");
// Salida: Ejecución de bucle: 10.2197265625ms
```

También puede utilizar estilos para resaltar sus salidas de depuración. Por ejemplo, puede utilizar `console.log("%cTexto a destacar", "color: red; font-weight: bold;")` para imprimir un mensaje en rojo y negrita. Esto puede ser útil para identificar rápidamente ciertas secciones de su código.

## Ver también

- [Documentación de Console API en MDN](https://developer.mozilla.org/es/docs/Web/API/Console)
- [Tutorial de debugging en TypeScript](https://www.digitalocean.com/community/tutorials/how-to-debug-node-js-with-the-built-in-visual-studio-code-debugger)