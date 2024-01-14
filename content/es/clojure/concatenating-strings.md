---
title:    "Clojure: Uniendo cadenas de texto"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Por qué

La concatenación de cadenas es una técnica esencial en la programación de Clojure. Permite combinar múltiples cadenas en una sola, creando una salida más legible y fácil de manejar.

## Cómo hacerlo

La concatenación de cadenas en Clojure es sencilla. Se puede lograr utilizando la función `str` y colocando cada cadena entre paréntesis. Veamos un ejemplo:

```Clojure
(str "¡Hola," "amigos" "!") ; output: ¡Hola, amigos!
```

Las cadenas también pueden ser variables, lo que permite una mayor flexibilidad en la creación de cadenas concatenadas. Por ejemplo:

```Clojure
(def nombre "María")
(def estado "feliz")

(str "¡Hola, " nombre "! Estoy " estado " de verte.") ; output: ¡Hola, María! Estoy feliz de verte.
```

## Profundizando

La función `str` en realidad es solo un atajo para la función` clojure.string/join`. Al utilizar esta función, se puede especificar un separador entre las cadenas concatenadas. Por defecto, el separador es un espacio en blanco, pero se puede cambiar según sea necesario. Por ejemplo:

```Clojure
(clojure.string/join "-" ["a" "b" "c"]) ; output: a-b-c
```

Además, Clojure también proporciona la función `format`, que permite una mayor personalización al concatenar cadenas. Esta función utiliza un formato similar al de la función `printf` en otros lenguajes de programación. Veamos un ejemplo:

```Clojure
(format "Tengo %d años de edad y mi nombre es %s." 25 "Juan") ; output: Tengo 25 años de edad y mi nombre es Juan.
```

## Ver también

Para obtener más información sobre la concatenación de cadenas en Clojure, consulta estos recursos:

- [Documentación oficial de Clojure](https://clojure.org/api/clojure.string)
- [Tutorial de Clojure de Programiz](https://www.programiz.com/clojure/string-manipulation)