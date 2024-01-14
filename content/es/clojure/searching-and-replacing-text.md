---
title:                "Clojure: Buscando y reemplazando texto"
simple_title:         "Buscando y reemplazando texto"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por qué

Reemplazar texto es una tarea común en programación. Permite modificar grandes cantidades de texto de forma rápida y eficiente. En este blog post, aprenderemos cómo realizar búsquedas y reemplazos de texto en Clojure.

## Cómo hacerlo

### Buscar y reemplazar un patrón específico

Podemos utilizar la función `clojure.string/replace` para buscar y reemplazar un patrón específico en una cadena de texto. Por ejemplo, si queremos reemplazar todas las letras mayúsculas por minúsculas en una oración, podemos hacer lo siguiente:

```Clojure
(clojure.string/replace "ESTE ES UN EJEMPLO" #"A-Z" fn [m] (clojure.string/lower-case m))
```

Esto nos dará como resultado "este es un ejemplo".

### Reemplazar todas las ocurrencias

Si deseamos reemplazar todas las ocurrencias de un patrón en una cadena de texto, podemos utilizar la función `clojure.string/replace-first` y un bucle `while`. El bucle continuará buscando y reemplazando el patrón hasta que ya no queden ocurrencias. 

```Clojure
(def texto "hola, hola, hola")
(while true
  (let [reemplazado (clojure.string/replace-first texto #"hola" "adiós")]
    (when (= texto reemplazado)
      (break))
    (def texto reemplazado))
  )

(prn texto) ; resultado: "adiós, adiós, adiós"
```

## Profundizando

A veces, puede ser necesario realizar búsquedas y reemplazos en archivos de texto. En ese caso, podemos utilizar la función `slurp` para leer el contenido del archivo y luego aplicar las funciones mencionadas anteriormente para realizar los reemplazos.

```Clojure
(def archivo (slurp "ejemplo.txt"))
(def archivo-modificado (clojure.string/replace archivo #"patrón" "reemplazo"))
(spit "ejemplo.txt" archivo-modificado)
```

## Ver también

- [Documentación oficial de strings en Clojure](https://clojure.org/guides/strings)
- [Guía interactiva de Clojure para principiantes](https://clojure.org/guides/getting_started)