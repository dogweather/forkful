---
title:                "Escritura de un archivo de texto"
date:                  2024-01-19
html_title:           "Bash: Escritura de un archivo de texto"
simple_title:         "Escritura de un archivo de texto"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Escribir un archivo de texto permite almacenar información de manera permanente. Los programadores lo hacen para guardar configuraciones, resultados de programas, o cualquier tipo de datos seriales.

## How to:
En Clojure, escribimos archivos de texto usando `spit`. Ejemplo:

```clojure
(spit "example.txt" "¡Hola, Clojure!")
```

Revisamos el contenido del archivo `example.txt`:

```clojure
(slurp "example.txt")
; => "¡Hola, Clojure!"
```

Para añadir más texto sin sobrescribir:

```clojure
(spit "example.txt" "Adiós, Clojure!" :append true)
```

El contenido de `example.txt` será:

```
¡Hola, Clojure!
Adiós, Clojure!
```

## Deep Dive
Clojure es un dialecto de Lisp; su función `spit` simplifica el proceso de escritura de archivos, que en otros lenguajes puede ser más verboso. Alternativas a `spit` incluyen usar Java interop (p. ej. `FileWriter`), esto puede ofrecer más control. `spit` es ideal para tareas directas y scripts rápidos, mientras que Java interop es preferido para requerimientos complejos.

## See Also
- Documentación oficial de Clojure: https://clojure.org/api/api
- Clojure for the Brave and True, una guía gratuita y completa para aprender Clojure: https://www.braveclojure.com/clojure-for-the-brave-and-true/
- Ejemplos y buenas prácticas para trabajar con archivos en Clojure: https://clojuredocs.org/clojure.core/spit
