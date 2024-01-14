---
title:    "Elm: Escribiendo a la salida de error estándar"
keywords: ["Elm"]
---

{{< edit_this_page >}}

**¿Por qué escribir a standard error en Elm?**

Si estás familiarizado con programación en Elm, sabrás que una de sus principales características es la seguridad en el tipo de datos. Esto significa que el compilador de Elm te ayudará a evitar errores de tipo en tu código. Sin embargo, hay ocasiones en las que puede ser útil escribir a standard error para obtener una salida de errores más detallada y comprensible.

**¿Cómo hacerlo?**

En Elm, podemos escribir a standard error utilizando la función `Debug.log`. Esta función toma dos argumentos: un string que describe el mensaje a imprimir y el valor que queremos imprimir. Veamos un ejemplo:

```Elm
import Debug exposing (log)

sumar : Int -> Int -> Int
sumar x y =
  x + y

main =
  let
    resultado = sumar 5 10
  in
    log "El resultado de la suma es:" resultado
```

En este código, estamos utilizando la función `sumar` para sumar dos números y luego utilizando `Debug.log` para imprimir el resultado de la suma a standard error. Cuando ejecutemos este código en la consola, veremos lo siguiente:

```
El resultado de la suma es: 15
```

Como puedes ver, hemos impreso la cadena de texto que especificamos en el primer argumento y el resultado de la suma en el segundo.

**Profundizando en la escritura a standard error**

Ahora, es importante tener en cuenta que la función `Debug.log` solo se ejecuta durante el proceso de compilación. Esto significa que cualquier llamada a esta función en tu código será eliminada una vez que se compile tu aplicación. Por lo tanto, no debes preocuparte por el rendimiento o impacto en tu aplicación.

Además, es importante tener en cuenta que solo debe utilizarse `Debug.log` para fines de depuración. Una vez que hayas encontrado y solucionado el error, es importante eliminar o comentar cualquier llamada a esta función en tu código final.

**Ver también**

- [Information about Debug.log from the Elm Guide](https://guide.elm-lang.org/debugging/debugging.html#writing-to-standard-error)
- [Elm language documentation for Debug](https://elm-lang.org/docs/debug)
- [Elm in Action book by Richard Feldman](https://www.manning.com/books/elm-in-action)