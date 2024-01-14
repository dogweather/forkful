---
title:                "Gleam: Impresión de salida de depuración"
programming_language: "Gleam"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## ¿Por qué imprimir mensajes de depuración en Gleam?

Imprimir mensajes de depuración es una técnica comúnmente utilizada en la programación para verificar el comportamiento de un programa en tiempo de ejecución. En Gleam, esta práctica es especialmente útil debido a la naturaleza funcional del lenguaje.

## Cómo imprimir mensajes de depuración en Gleam

Para imprimir mensajes de depuración en Gleam, podemos utilizar la función `debug!`, que acepta una cadena como argumento. Veamos un ejemplo:

```Gleam
let nombre = "María"
debug!("El nombre es: {}", [nombre])
```

Esto imprimirá en la consola el mensaje "El nombre es: María". También podemos imprimir valores de variables utilizando la cadena de formato `{}` y la lista de argumentos correspondientes. Por ejemplo:

```Gleam
let x = 5
let y = 10
debug!("La suma de x e y es: {}", [x + y])
```

Esto imprimirá en la consola "La suma de x e y es: 15".

## Profundizando en la impresión de mensajes de depuración

Además de utilizar la función `debug!`, también podemos configurar el nivel de depuración en Gleam para imprimir mensajes en diferentes momentos durante la ejecución del programa. Esto puede ser útil para identificar errores o problemas en un código complejo. Otra técnica útil es utilizar `println!`, que imprime mensajes en la consola sin interrupciones de formato.

En resumen, imprimir mensajes de depuración en Gleam puede facilitar el proceso de desarrollo y depuración de programas. Sin embargo, es importante tener en cuenta que estos mensajes deben ser eliminados antes de enviar el código a producción.

## Ver también

- Documentación de Gleam: https://gleam.run/
- Ejemplos de código de Gleam: https://github.com/gleam-lang
- Guía de depuración en Gleam: https://gleam.run/book/tour/getting-started.html#debugging