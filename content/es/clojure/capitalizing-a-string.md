---
title:                "Capitalizando una cadena de texto"
html_title:           "Clojure: Capitalizando una cadena de texto"
simple_title:         "Capitalizando una cadena de texto"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

**## ¿Qué es & Por qué?**

Capitalizar una cadena se refiere a hacer que la primera letra de cada palabra en una cadena sea mayúscula. Los programadores a menudo hacen esto para mejorar la legibilidad en la presentación de texto.

**## Cómo hacerlo:**

En Clojure, utilizamos la función `clojure.string/capitalize` para capitalizar una cadena. Aquí tienes un ejemplo.

```Clojure
(ns ejemplocapital.StringCap
  (:require [clojure.string :as str]))

(defn -main []
  (println (str/capitalize "hola mundo")))
```

Cuando ejecutes este código, obtendrás el siguiente resultado:

```Clojure
Hola Mundo
```

**## Detalles más profundos:**

- **Contexto histórico:** Clojure, a pesar de ser relativamente joven (primera versión en 2007), incorporó las funciones de manipulación de cadenas desde sus primeras versiones. La función `clojure.string/capitalize` forma parte de estas funciones.

- **Alternativas:** En lugar de usar la función `clojure.string/capitalize`, podrías hacerlo manualmente con las funciones `clojure.string/lower-case` y `clojure.string/upper-case` combinadas con `clojure.string/split` y `clojure.string/join`.

- **Detalles de implementación:** La función `clojure.string/capitalize` trabaja splitteando la cadena en palabras, convierte la primera letra de cada palabra a mayúscula y el resto a minúscula, y luego une las palabras juntas de nuevo.

```Clojure
(defn my-capitalize [s]
    (->> s
         (str/split #"\s")
         (map #(str (str/upper-case (first %1)) 
                     (str/lower-case (rest %1))))
         (str/join " ")))

(println (my-capitalize "hola mundo")) ;"Hola Mundo"
```

**## Ver También:**

- Documentación oficial de Clojure: [https://clojure.org/](https://clojure.org/) 
- Clojure.string documentation: [https://clojuredocs.org/clojure.string](https://clojuredocs.org/clojure.string) 
- Guía de Clojure para principiantes: [https://www.braveclojure.com/](https://www.braveclojure.com/)