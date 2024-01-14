---
title:                "Clojure: Encontrando la longitud de una cadena"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por qué

Alprogramar en Clojure, a menudo nos encontramos con la necesidad de encontrar la longitud de una cadena de texto. Esto es útil para realizar diferentes tipos de operaciones, como validar entradas del usuario o manipular datos de una manera más específica.

## Cómo hacerlo

Para obtener la longitud de una cadena en Clojure, podemos utilizar la función ```count```, que nos devuelve el número de elementos en una secuencia. En una cadena, los elementos serían los caracteres.

Veamos un ejemplo de cómo utilizar esta función:

```Clojure
(count "Hola mundo") ; Devuelve 10 
```

En este caso, la cadena "Hola mundo" tiene 10 caracteres, por lo que el resultado de la función ```count``` es 10.

Otra forma de obtener la longitud de una cadena es utilizando el método ```length``` de la clase ```String```. Este método es más eficiente ya que cuenta directamente el número de caracteres en la cadena, mientras que la función ```count``` tendrá que recorrer toda la secuencia.

Veamos cómo utilizar el método ```length```:

```Clojure
(.length "Hello world") ; Devuelve 11 
```

## Profundizando

Al utilizar la función ```count``` para obtener la longitud de una cadena, es importante tener en cuenta que si la cadena contiene caracteres unicode, estos serán contados como un solo elemento. Esto puede causar diferencias en el resultado si se está trabajando con diferentes idiomas.

Es importante también considerar que en Clojure, las cadenas son inmutables, lo que significa que no pueden ser modificadas una vez creadas. Sin embargo, se pueden utilizar funciones para manipular y obtener información de las cadenas, como es el caso de ```count``` y ```length```.

## Ver también

- [Documentación oficial de Clojure sobre la función count](https://clojuredocs.org/clojure.core/count)
- [Documentación oficial de Java sobre el método length](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#length--)