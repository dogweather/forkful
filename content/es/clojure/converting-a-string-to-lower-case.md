---
title:                "Clojure: Convirtiendo una cadena a minúsculas"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por qué

En programación, a menudo necesitamos manipular cadenas de texto en nuestro código. Una tarea común es convertir una cadena a minúsculas. Esto puede ser útil para comparar cadenas sin importar su formato o para mostrar resultados en un formato estandarizado. En este post, aprenderemos cómo hacer esto en Clojure.

## Cómo hacerlo

Para convertir una cadena a minúsculas en Clojure, podemos utilizar la función `clojure.string/lower-case`. Veamos un ejemplo:

```Clojure
(def cadena "Hola Mundo")
(clojure.string/lower-case cadena)
```

El resultado de esta operación sería `"hola mundo"`. Como podemos ver, la función `lower-case` convierte todas las letras a minúsculas. Si queremos conservar la capitalización original, podemos utilizar la función `lower-case-first`. Por ejemplo:

```Clojure
(def cadena "Hola Mundo")
(clojure.string/lower-case-first cadena)
```

En este caso, el resultado sería `"hola mundo"`, conservando la "H" mayúscula.

También podemos utilizar la función `clojure.string/upper-case` para convertir una cadena a mayúsculas.

```Clojure
(def cadena "Hola Mundo")
(clojure.string/upper-case cadena)
```

El resultado sería `"HOLA MUNDO"`.

## Profundizando

Internamente, la función `lower-case` utiliza la función `character`, que devuelve el carácter Unicode correspondiente a la cadena dada. Luego, utiliza la función `lower-case` en el carácter para obtener la versión en minúsculas. Este proceso se repite para cada carácter de la cadena.

Es importante tener en cuenta que la función `lower-case` siempre devuelve una nueva cadena. Esto significa que no podemos modificar una cadena existente y convertirla a minúsculas, sino que debemos asignar el resultado a una nueva variable.

## Ver también

- [Documentación oficial de Clojure sobre la función lower-case](https://clojuredocs.org/clojure.string/lower-case)
- [Ejercicios prácticos para practicar con la conversión de cadenas en Clojure](https://www.4clojure.com/problem/4)
- [Guía completa de Clojure para principiantes](https://www.tutorialspoint.com/clojure/index.htm)