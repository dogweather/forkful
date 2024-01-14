---
title:                "Clojure: Eliminar caracteres que coinciden con un patrón"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## ¿Por qué?

A veces, al trabajar con cadenas de texto en Clojure, te encuentras con caracteres que no necesitas. Estos pueden ser espacios en blanco, números, símbolos, o cualquier otro carácter que no cumpla con cierto patrón. En estos casos, puede ser útil saber cómo eliminar esos caracteres para obtener una cadena más limpia y legible.

## Cómo hacerlo

Para eliminar caracteres que coincidan con un patrón, utilizamos la función `replace-first` de la librería core de Clojure. Por ejemplo, si queremos eliminar todos los espacios en blanco de una cadena, podemos hacer lo siguiente:

```Clojure
(require '[clojure.string :as str])

(def texto "Hola   Mundo")

(str/replace-first texto #"\s+" "")
; => "HolaMundo"
```

En el código anterior, la función `replace-first` toma dos argumentos: la cadena en la que queremos buscar y el patrón que queremos eliminar. En este caso, usamos la expresión regular `#"\s+"` para representar uno o más espacios en blanco. Luego, reemplazamos esos espacios con una cadena vacía `""`.

Podemos utilizar esta técnica para eliminar otros tipos de caracteres también. Por ejemplo, si queremos eliminar todos los números de una cadena, podemos usar la expresión regular `#"\d+"` para representar cualquier digito numérico.

```Clojure
(def texto "Hoy es 19 de abril de 2021")

(str/replace-first texto #"\d+" "")
; => "Hoy es de abril de"
```

## Profundizando

La función `replace-first` de Clojure también nos permite utilizar una función como segundo argumento, en lugar de un patrón. Esto significa que podemos realizar una acción sobre los caracteres que coinciden con el patrón antes de eliminarlos.

Por ejemplo, si queremos eliminar todas las letras mayúsculas de una cadena, podemos usar la función `clojure.string/upper-case` en lugar de una expresión regular:

```Clojure
(def texto "¡HOLA, MUNDO!")

(str/replace-first texto clojure.string/upper-case "")
; => "¡, !"
```

En este caso, la función `upper-case` se aplica a cada letra mayúscula encontrada en la cadena antes de eliminarla.

## Ver también

- Documentación oficial de `replace-first`: https://clojuredocs.org/clojure.string/replace-first
- Tutorial de expresiones regulares en Clojure: http://julianromera.com/2016/12/24/expresiones-regulares-en-clojure.html
- Ejemplos de uso de `replace-first`: https://puredanger.github.io/tech.puredanger.com/2010/04/21/replacements/