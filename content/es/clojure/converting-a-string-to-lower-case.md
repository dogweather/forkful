---
title:                "Convirtiendo una cadena a minúsculas"
html_title:           "Bash: Convirtiendo una cadena a minúsculas"
simple_title:         "Convirtiendo una cadena a minúsculas"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Conversión de una cadena a minúsculas en Clojure

## ¿Qué y por qué?
La conversión de una cadena a minúsculas en la programación es el proceso de cambiar todas las letras mayúsculas en una cadena a su forma minúscula correspondiente. Los desarrolladores lo hacen para permitir la comparación precisa de las cadenas, sin tener en cuenta las diferencias de mayúsculas y minúsculas.

## Cómo hacerlo: 
Clojure hace que este proceso sea bastante simple con la función 'clojure.string/lower-case':

```Clojure
(require '[clojure.string :as str])

(str/lower-case "Hello, World!")
```

Este código convierte la cadena "Hello, World!" a minúsculas. La salida será:

```Clojure
"hello, world!"
```

## Buceo profundo

1. **Contexto histórico**: En las primeras etapas de la informática, la distinción entre mayúsculas y minúsculas no era algo que todos los sistemas informáticos mantenían. Con el tiempo, la necesidad de esta distinción se hizo más importante y los lenguajes de programación incorporaron formas de manipular las letras en función de su caso.

2. **Alternativas**: Aunque la biblioteca 'clojure.string' proporciona la función 'lower-case' para esta tarea, también podrías implementar tu propia función utilizando la función 'map' y el método 'Character/toLowerCase'. 

```Clojure
(apply str (map clojure.lang.Character/toLowerCase "Hello, World!"))
```
3. **Detalles de implementación**: La función 'str/lower-case' de Clojure utiliza Java String 'toLowerCase' internamente, que considera las configuraciones de localización. Esto significa que la conversión a minúsculas tendrá en cuenta las reglas del idioma del entorno de la máquina.

## Ver también
* Documentación de clojure.string/lower-case: https://clojuredocs.org/clojure.string/lower-case
* Documentación de Java String toLowerCase: https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#toLowerCase()