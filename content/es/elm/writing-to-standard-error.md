---
title:                "Elm: Escribiendo en el error estándar"
programming_language: "Elm"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por qué

Escribir a la salida de error estándar puede ser una tarea importante en la programación de Elm. Al hacerlo, podemos imprimir mensajes de error o advertencia para ayudarnos a depurar nuestro código y mejorar la calidad de nuestras aplicaciones. Además, escribir a la salida de error también puede ser útil para comunicarse con otros miembros del equipo durante el desarrollo.

## Cómo hacerlo

Para escribir a la salida de error estándar en Elm, podemos utilizar la función `Debug.log` de la biblioteca `Debug`. Esta función toma dos argumentos: una cadena de texto y un valor. La cadena de texto será el mensaje que se imprimirá en la salida de error y el valor será cualquier cosa que queramos imprimir, como una cadena de texto, un número o incluso una estructura más compleja como una lista o un registro.

Veamos un ejemplo de cómo usar `Debug.log` en nuestro código:

```elm
import Debug

numero : Int
numero = 42

Debug.log "Mi número favorito es" numero
```

Al ejecutar este código, veremos en la salida de error:

```bash
Mi número favorito es 42
```

Podemos usar esta técnica para imprimir valores en momentos importantes de nuestra aplicación o para encontrar errores en nuestro código.

## Profundizando

Además de `Debug.log`, también podemos utilizar la función `Debug.crash` para imprimir mensajes de error y finalizar la ejecución de nuestra aplicación. Esta función toma una cadena de texto como argumento y la imprime en la salida de error antes de finalizar el programa.

Otra técnica útil es utilizar la función `Debug.todo`. Esta función no hace nada, pero nos permite tener placeholders en nuestro código para recordar que hay algo que aún no hemos implementado o que necesitamos trabajar en esa parte en el futuro.

Es importante tener en cuenta que el uso excesivo de `Debug.log`, `Debug.crash` o `Debug.todo` puede afectar el rendimiento de nuestra aplicación, por lo que es importante usarlos con moderación y eliminarlos una vez que ya no sean necesarios.

## Mira también

- [Documentación de la Biblioteca de Depuración de Elm](https://package.elm-lang.org/packages/elm-lang/core/latest/Debug)
- [Artículo sobre depuración en Elm](https://medium.com/@joeyvx/debugging-in-elm-fbf290797550)
- [Vídeo tutorial sobre depuración en Elm](https://www.youtube.com/watch?v=2426qIWrPfU)