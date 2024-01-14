---
title:                "Javascript: Imprimiendo salida de depuración"
programming_language: "Javascript"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/javascript/printing-debug-output.md"
---

{{< edit_this_page >}}

##Por qué: 
Saber cómo imprimir mensajes de depuración en tu código Javascript es una habilidad importante para los desarrolladores. Te permite identificar y resolver errores en tu código de manera más eficiente, lo que ahorra tiempo y mejora la calidad de tu programa.

##Cómo hacerlo:
Para imprimir mensajes de depuración en Javascript, puedes utilizar el método `console.log()`. Este método toma un argumento y lo imprime en la consola del navegador. Puedes utilizarlo para imprimir cualquier tipo de dato, como una cadena, un número o una variable.

Por ejemplo:

```JavaScript
let name = "Juan";
console.log("Hola " + name);

// Output: Hola Juan
```

También puedes utilizar `console.log()` para imprimir el valor de una expresión o una condición dentro de un bucle. Esto te permite verificar si tu código está funcionando correctamente.

```Javascript
for(let i = 0; i < 5; i++){
  console.log(i);
}

// Output: 0, 1, 2, 3, 4 
```

Otra forma de imprimir mensajes de depuración es utilizando `console.dir()`, que te permite ver los detalles de un objeto en la consola. Este método es especialmente útil cuando necesitas inspeccionar objetos complejos o anidados.

```Javascript
let person = {
  name: "Maria",
  age: 25,
  address: {
    street: "Calle Principal",
    city: "Madrid"
  }
};

console.dir(person);

/* Output:
  {
    name: "Maria",
    age: 25,
    address: {
      street: "Calle Principal",
      city: "Madrid"
    }
  }
*/
```

##Profundizando:
Además de `console.log()` y `console.dir()`, existen otras formas de imprimir mensajes de depuración en Javascript, como `console.warn()` y `console.error()`, que te permiten mostrar mensajes de advertencia y errores respectivamente.

También puedes utilizar `console.table()` para imprimir datos en formato de tabla, lo que facilita la visualización y comparación de la información.

Si necesitas imprimir mensajes de depuración en diferentes niveles de detalle, puedes utilizar `console.group()` y `console.groupEnd()`, que te permiten agrupar y expandir o contraer secciones de tus mensajes de depuración en la consola.

Además, hay otras herramientas y técnicas que puedes utilizar para imprimir mensajes de depuración en Javascript, como las extensiones de navegador o la depuración mediante el editor de código.

##Ver también:
- [Usando la consola del navegador para depurar Javascript](https://developer.mozilla.org/es/docs/Learn/JavaScript/First_steps/What_went_wrong)
- [Herramientas de depuración en Chrome](https://developers.google.com/web/tools/chrome-devtools/javascript)
- [Usando `console.log()` como un profesional](https://medium.com/better-programming/how-to-console-log-like-a-pro-ac2c75aa60f8)