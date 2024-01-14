---
title:    "Clojure: Capitalizando una cadena"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Por qué 

Capitalizar una cadena de caracteres es una tarea común en la programación, especialmente en el procesamiento de datos y en la creación de interfaces de usuario. A menudo es necesario para mejorar la legibilidad y presentación de la información al usuario final.

## Cómo hacerlo

Para capitalizar una cadena de caracteres en Clojure, podemos utilizar la función `clojure.string/capitalize`. Esta función toma una cadena de caracteres como argumento y devuelve una nueva cadena con la primera letra en mayúsculas y el resto en minúsculas. Veamos un ejemplo de cómo usarla:

```Clojure
(clojure.string/capitalize "hola mundo") ;; salida: "Hola mundo"
(clojure.string/capitalize "CLOJURE") ;; salida: "Clojure"
(clojure.string/capitalize "eL nUmErO 42 eS eL mÁs gRaNde") ;; salida: "El número 42 es el más grande"
```

También podemos utilizar la función `upper-case` para capitalizar una cadena completa:

```Clojure
(upper-case "java es una isla") ;; salida: "JAVA ES UNA ISLA"
```

## Profundizando

Detrás de las funciones `clojure.string/capitalize` y `upper-case` se encuentra el concepto de "case conversion" o conversión de mayúsculas y minúsculas. En Clojure, los caracteres tienen un valor numérico llamado código de carácter, que representa su posición en la tabla ASCII. Los caracteres en mayúsculas tienen códigos numéricos más bajos que los caracteres en minúsculas. 

Cuando utilizamos la función `clojure.string/capitalize`, lo que realmente está sucediendo es que se toma el primer caracter de la cadena y se le agrega la constante numérica 32, convirtiéndolo así en su equivalente en minúsculas. Por lo tanto, para cada letra en mayúsculas, su código ASCII se transforma en el código ASCII correspondiente en minúsculas y viceversa.

Esto también explica por qué, al capitalizar una cadena con números o símbolos, estos no se ven afectados, ya que no tienen códigos ASCII asociados con las mayúsculas o minúsculas.

## Ver también

- [ClojureDocs: clojure.string](https://clojuredocs.org/clojure.string)
- [Ejercicios de programación en Clojure](https://www.4clojure.com/problems)
- [Introducción a programación funcional en Clojure](https://clojure.org/guides/getting_started)