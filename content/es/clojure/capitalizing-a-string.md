---
title:                "Capitalizando una cadena de texto"
html_title:           "Arduino: Capitalizando una cadena de texto"
simple_title:         "Capitalizando una cadena de texto"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Capitalizar una cadena significa convertir la primera letra de cada palabra en mayúscula mientras el resto de las letras se quedan en minúscula. Los programadores lo hacen para formatear texto de manera que sea más legible o cumpla con ciertas reglas de presentación.


## How to:
En Clojure, puedes capitalizar una cadena usando la función `clojure.string/capitalize`. Aquí te muestro cómo:

```Clojure
(require '[clojure.string :as str])

;; Capitalizar una palabra
(println (str/capitalize "hola"))         ; "Hola"

;; Capitalizar cada palabra en una cadena
(println (mapv str/capitalize (str/split "bienvenidos a clojure" #" ")))  ; ["Bienvenidos" "A" "Clojure"]
```

El resultado del código anterior será:

```
Hola
[Bienvenidos A Clojure]
```

Ten en cuenta que `str/capitalize` solo hace mayúscula la primera letra de la cadena completa; si quieres capitalizar cada palabra individualmente, necesitas dividir la cadena primero.

## Deep Dive
Capitalizar cadenas no es una invención nueva, viene desde la era de la máquina de escribir. Es una norma en la mayoría de los idiomas, particularmente en títulos o cuando se quiere destacar algo como nombres propios.

En Clojure, la función `str/capitalize` solo capitaliza la primera letra de la cadena completa. No hay una función incorporada para capitalizar todas las palabras de una cadena (también conocido como "title case"). Necesitas dividir la cadena manualmente en palabras, capitalizarlas y luego unirlas, como mostramos en el ejemplo anterior.

Otras lenguajes como Python ofrecen métodos como `.title()` que hacen todo esto automáticamente. Sin embargo, Clojure favorece un enfoque de componibilidad, lo cual significa que uno mismo construye funciones más complejas a partir de otras más sencillas.

Algunas implementaciones para capitalizar cadenas pueden ser ineficientes si están mal escritas. Por ejemplo, si usamos `map str/capitalize` en lugar de `mapv`, no obtendremos una vector de vuelta, sino una secuencia lazy, lo cual podría no ser lo esperado.

## See Also
Para más información sobre el manejo de cadenas en Clojure:

- Documentación oficial de `clojure.string`: [clojure.github.io/clojure/clojure.string-api.html](https://clojure.github.io/clojure/clojure.string-api.html)

También puedes explorar bibliotecas de terceros para funcionalidades de string más avanzadas o específicas. Recuerda siempre leer la documentación y los ejemplos para entender completamente lo que el código hace y cómo integrarlo con tu trabajo actual.