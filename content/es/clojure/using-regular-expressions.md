---
title:    "Clojure: Utilizando expresiones regulares"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Por qué usar expresiones regulares en Clojure

Las expresiones regulares son herramientas poderosas para manipular y buscar cadenas de texto en un programa. En Clojure, pueden ser útiles para validar entradas de usuario, transformar datos y muchas otras tareas. 

## Cómo utilizarlas

Para utilizar expresiones regulares en Clojure, primero es necesario incluir el namespace `clojure.string` en tu código:

```Clojure
(require '[clojure.string :as str])
```

Luego, puedes utilizar la función `re-find` para buscar una expresión regular en una cadena de texto y devolver la primera coincidencia encontrada:

```Clojure
(str/re-find #"^[AEIOU]" "Hola") ; Devuelve "H"
```

También puedes utilizar la función `re-seq` para buscar todas las coincidencias en una cadena y devolver una secuencia de ellas:

```Clojure
(str/re-seq #"\d+" "Tengo 3 gatos y 2 perros") ; Devuelve la secuencia (3 2)
```

Además, Clojure tiene funciones específicas para trabajar con expresiones regulares, como `re-matches`, `re-groups` y `re-pattern`. Puedes consultar más detalles en la documentación oficial de Clojure.

## Profundizando en expresiones regulares

Las expresiones regulares en Clojure siguen la misma sintaxis que en otros lenguajes, por lo que si ya tienes experiencia con ellas, no deberías tener problemas. Sin embargo, si eres nuevo en ellas, puede ser útil conocer algunos elementos básicos:

- `^` y `$`: Representan el inicio y fin de una cadena, respectivamente.
- `.`: Representa cualquier carácter.
- `*`, `+` y `?`: Indican repetición de patrones, siendo `*` cero o más veces, `+` una o más veces, y `?` cero o una vez.
- `[]`: Representan un conjunto de carácteres, pudiendo ser especificados individualmente o a través de un rango (por ejemplo, `[a-z]` incluye todas las letras minúsculas).
- `|`: Indica alternativa, es decir, puede ser uno o el otro.
- `(  )`: Sirven para agrupar patrones.

Un buen ejercicio para practicar es jugar con diferentes combinaciones de estos elementos y ver cómo afecta a los resultados de las expresiones regulares.

# Ver También

- [Documentación oficial de expresiones regulares en Clojure](https://clojure.org/reference/strings#_regular_expressions)
- [Tutorial de expresiones regulares en Clojure](https://www.theserverside.com/tutorial/A-tutorial-on-how-to-use-Clojure-regular-expressions)
- [Ejemplos de expresiones regulares en Clojure](https://gist.github.com/shriphani/5392592)