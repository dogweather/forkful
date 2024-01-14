---
title:    "Clojure: Eliminando caracteres que coinciden con un patrón."
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Por qué
A veces, cuando se trabaja con cadenas de texto en un programa, es posible que se necesite eliminar ciertos caracteres que coinciden con un patrón específico. La solución a este problema es utilizar la función "subs" en Clojure para eliminar los caracteres de una cadena de texto según un patrón determinado.

## Cómo hacerlo
Para utilizar la función "subs" en Clojure, simplemente se debe utilizar la siguiente sintaxis:

```Clojure
(subs string start end?)
```

donde "string" es la cadena de texto en la que se va a trabajar, "start" es el índice del primer carácter a eliminar y "end" (opcional) es el índice del último carácter a eliminar.

Por ejemplo, si queremos eliminar los caracteres del 2 al 5 de una cadena de texto, se utilizaría la siguiente función:

```Clojure
(subs "abcdefg" 2 5)
```

La salida de este código sería la cadena de texto "adfg", ya que se han eliminado los caracteres del índice 2 al 5.

También es posible utilizar patrones más complejos con el uso de expresiones regulares. Por ejemplo, si se desea eliminar todos los dígitos de una cadena de texto, se puede utilizar la siguiente función:

```Clojure
(subs "abc123def456" #"\d") ;#"\d" es una expresión regular que representa cualquier dígito
```

La salida de este código sería la cadena de texto "abcdef", ya que se han eliminado todos los dígitos de la cadena original.

## Profundizando
La función "subs" en Clojure utiliza índices para determinar qué caracteres deben ser eliminados de una cadena de texto. Estos índices comienzan en 0, por lo que el primer carácter de una cadena de texto tiene un índice de 0 y el último carácter tiene un índice de (n-1), donde n es la longitud de la cadena.

También es importante tener en cuenta que la función "subs" no modifica la cadena de texto original, sino que devuelve una nueva cadena con los caracteres eliminados. Por lo tanto, es importante asignar el resultado de la función a una nueva variable o imprimirlo para ver el resultado.

## Ver también
- [La documentación oficial de Clojure para la función "subs"](https://clojuredocs.org/clojure.core/subs)
- [Un tutorial sobre expresiones regulares en Clojure](https://www.youtube.com/watch?v=WIoUtNS7n1A)
- [Ejemplo de uso de la función "subs" en un programa en Clojure](https://github.com/learn-clojure/Learn-Clojure/blob/master/4._Strings.md#excercise-18)