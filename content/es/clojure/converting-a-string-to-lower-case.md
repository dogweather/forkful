---
title:                "Clojure: Convirtiendo una cadena en minúsculas"
simple_title:         "Convirtiendo una cadena en minúsculas"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## ¿Por qué convertir una cadena a minúsculas?

Al trabajar con cadenas de texto, a menudo nos encontramos con la necesidad de tener una cadena en minúsculas para poder realizar operaciones como comparaciones o búsquedas de forma efectiva. Convertir una cadena a minúsculas es una tarea común en la programación y hoy te mostraremos cómo hacerlo en Clojure.

## Cómo hacerlo

Para convertir una cadena a minúsculas en Clojure, podemos utilizar la función `lower-case` de la librería `clojure.string`. A continuación, te mostramos un ejemplo de cómo usarla:

```Clojure
(require '[clojure.string :as str])
(str/lower-case "HOLA MUNDO") ; este código devuelve "hola mundo"
```

También podemos utilizar la función `lower-case` directamente en una cadena, de esta forma:

```Clojure
(lower-case "HOLA MUNDO") ; este código devuelve "hola mundo"
```

Como puedes ver, ambas formas son válidas y nos dan el mismo resultado. Sin embargo, es importante recordar que esta función solo funciona con letras mayúsculas y no afectará a otros caracteres como números o símbolos.

## Profundizando

Si nos adentramos un poco más en la función `lower-case`, podemos ver que esta utiliza la función `Character.toLowerCase` de Java para realizar la conversión. Esto significa que, en realidad, esta función no es específica de Clojure, sino que es parte de la librería estándar de Java.

## Ver también

- Documentación oficial de la función `lower-case`: https://clojuredocs.org/clojure.string/lower-case
- Más información sobre la función `Character.toLowerCase`: https://docs.oracle.com/javase/8/docs/api/java/lang/Character.html#toLowerCase-char-
- Tutorial básico de Clojure en español: https://javaparacafe.com/clojure-tutorial-espanol/