---
title:    "Clojure: Eliminando caracteres que coinciden con un patrón"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Por qué
En el mundo de la programación, a menudo nos encontramos con la necesidad de filtrar o manipular cadenas de texto. Una de las tareas comunes en este sentido es la eliminación de caracteres que coinciden con un patrón específico. En este post, hablaremos sobre cómo hacer esto utilizando Clojure.

## Cómo hacerlo
Para eliminar caracteres que coinciden con un patrón en Clojure, utilizamos la función `clojure.string/replace` y le pasamos el patrón y la cadena de texto que queremos manipular. Veamos un ejemplo en código:

```clojure
(require '[clojure.string :as str])

(str/replace "Hola mundo!" #"o" "") ; salida: "Hla mund!"
```

En este ejemplo, usamos la función `replace` para eliminar todas las letras "o" de la cadena "Hola mundo!". El símbolo `#""` antes de la letra "o" indica que estamos buscando un patrón y no solo un carácter específico. Podemos también especificar un patrón más complejo utilizando expresiones regulares.

```clojure
(str/replace "123abc456def" #"\d+" "") ; salida: "abcdef"
```

En este caso, utilizamos `\d+` como patrón, que representa cualquier secuencia de uno o más dígitos numéricos. La función `replace` eliminó entonces todos los números de la cadena original.

## Profundizando
La función `replace` utiliza internamente la función `clojure.string/replace-first`, que solo reemplaza la primera ocurrencia del patrón encontrado en la cadena. Sin embargo, podemos usar la función `replace` con un tercer parámetro opcional para especificar cuántas ocurrencias queremos reemplazar.

```clojure
(str/replace "111222333" #"2" "4" 2) ; salida: "111444333"
```

En este caso, le dijimos a la función `replace` que solo reemplace las dos primeras ocurrencias del patrón "2" por "4". Además, si queremos eliminar los caracteres que no coinciden con el patrón en lugar de reemplazarlos por uno nuevo, podemos simplemente pasar una cadena vacía como segundo parámetro.

# Ver también
- [Documentación de Clojure: clojure.string](https://clojuredocs.org/clojure.string)
- [Expresiones regulares en Clojure](https://clojuredocs.org/clojure.repl/doc/clojure.repl/source-fn)