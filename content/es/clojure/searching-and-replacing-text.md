---
title:    "Clojure: Buscando y reemplazando texto"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Por qué

La búsqueda y reemplazo de texto es una tarea común en el desarrollo de software, especialmente cuando se trabaja con grandes cantidades de datos. En el lenguaje de programación Clojure, existen diversas herramientas y técnicas que facilitan esta tarea y hacen que sea más eficiente y efectiva. En esta publicación, exploraremos cómo realizar búsqueda y reemplazo de texto en Clojure y cómo puede ser útil en tu trabajo diario.

## Cómo hacerlo

Para buscar y reemplazar texto en Clojure, utilizamos la función `clojure.string/replace` que toma tres argumentos: la cadena original, la cadena a reemplazar y la cadena de reemplazo. Por ejemplo, si queremos reemplazar todas las apariciones de "hola" con "adiós" en una cadena, podemos hacerlo de la siguiente manera:

```Clojure
(clojure.string/replace "Hola, Juan!" "hola" "adiós")
```

Este código producirá la cadena "Adiós, Juan!" como resultado. También podemos utilizar expresiones regulares para buscar patrones específicos en una cadena. Por ejemplo, si queremos convertir todas las letras mayúsculas en minúsculas en una cadena, podemos hacerlo de la siguiente manera:

```Clojure
(clojure.string/replace "Hola MUNDO!" #"[A-Z]" #(str/lower-case %))
```

Este código utilizará una expresión regular para buscar todas las letras mayúsculas en la cadena y las reemplazará con su versión en minúsculas. El resultado será la cadena "hola mundo!".

## Profundizando

Una de las características más útiles de la función `clojure.string/replace` es que también puede tomar una función como argumento de reemplazo. Esto significa que podemos realizar cualquier tipo de manipulación en el texto antes de reemplazarlo en la cadena original. Por ejemplo, si queremos reemplazar todos los números en una cadena con su versión en binario, podemos hacer lo siguiente:

```Clojure
(clojure.string/replace "123, 456, 789" #"\d+" #(Integer/toBinaryString (Integer/parseInt %)))
```

Este código utilizará una expresión regular para buscar todos los números en la cadena y luego utilizará la función `Integer/toBinaryString` para convertirlos en su equivalente en binario. El resultado será la cadena "1111011, 111001000, 1100010101".

Otra herramienta útil para la búsqueda y reemplazo de texto en Clojure es la biblioteca `clojure.string.regex`. Esta biblioteca proporciona una variedad de funciones para trabajar con expresiones regulares, lo que puede ser muy útil para casos más complejos de búsqueda y reemplazo. Puedes explorar más a fondo esta biblioteca y sus funciones en la documentación oficial de Clojure.

## Ver también

- Documentación oficial de Clojure para la función `clojure.string/replace`: https://clojuredocs.org/clojure.string/replace
- Documentación oficial de Clojure para la biblioteca `clojure.string.regex`: https://clojuredocs.org/clojure.string.regex