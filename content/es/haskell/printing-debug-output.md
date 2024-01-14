---
title:                "Haskell: Impresión de resultados de depuración"
simple_title:         "Impresión de resultados de depuración"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por qué 

Imagínate que estás construyendo una aplicación Haskell y todo parece estar funcionando bien, hasta que de repente, algo no funciona como debería. ¿Cómo puedes encontrar el error en tu código? Esta es donde el imprimir la salida de depuración puede ser útil. Imprimir mensajes de depuración en tu código te ayuda a identificar dónde está el error y a entender mejor cómo funciona tu programa.

## Cómo 

Para imprimir mensajes de depuración en Haskell, puedes utilizar la función `print` o la función `trace` del módulo `Debug.Trace`. Estas funciones imprimirán cualquier valor que les pases como argumento. Por ejemplo:

```Haskell
x = 5
-- Imprime el valor de x
print x
-- Imprime un mensaje junto con el valor de x
trace "El valor de x es: " x
```

La salida para ambos sería `5`, pero con `trace` también verías el mensaje `"El valor de x es: "` impreso antes del número.

## Deep Dive 

Además de la función `print` y `trace`, también puedes utilizar el ejecutable `ghci` para imprimir mensajes de depuración en tiempo real mientras estás haciendo pruebas en tu código. Con `ghci`, puedes llamar a funciones y ver su salida directamente en tu terminal. Por ejemplo:

```Haskell
ghci
Prelude> let x = 5
-- Imprime el valor de x
Prelude> x
5
-- Imprime un mensaje junto con el valor de x
Prelude> trace "El valor de x es: " x
El valor de x es: 5
```

Esto es especialmente útil si estás trabajando en un proyecto más grande y necesitas imprimir múltiples valores para encontrar el error.

## Ver también 

- [Documentación de `Debug.Trace` en Hackage](https://hackage.haskell.org/package/base-4.15.0.0/docs/Debug-Trace.html)
- [Tutorial de depuración en Haskell de School of Haskell](https://www.schoolofhaskell.com/school/advanced-haskell/debugging-haskell)
- [Artículo sobre depuración en Haskell de Medium](https://medium.com/@bea812/debunking-the-myths-around-debugging-in-haskell-c2f818d9faca)