---
title:                "Escribiendo un archivo de texto"
html_title:           "Clojure: Escribiendo un archivo de texto"
simple_title:         "Escribiendo un archivo de texto"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué escribir un archivo de texto

Escribir un archivo de texto es una forma básica pero útil de almacenar datos en un formato legible para los humanos y las computadoras. Puedes usarlo para guardar información como notas, listas de tareas, configuraciones y más.

## Cómo hacerlo

Para escribir un archivo de texto en Clojure, primero debes importar el módulo `java.io`, que contiene las funciones necesarias para trabajar con archivos. Puedes hacerlo de la siguiente manera:

```Clojure
(require '[clojure.java.io :as io])
```

Una vez que hayas importado el módulo, puedes usar la función `spit` para crear un archivo y escribir en él. Esta función acepta dos parámetros: la ruta del archivo y el contenido que deseas escribir.

```Clojure
(spit "mi-archivo.txt" "¡Hola mundo!")
```

Si ejecutas este código, se creará un archivo llamado `mi-archivo.txt` con el contenido `¡Hola mundo!`.

También puedes usar la función `slurp` para leer un archivo de texto. Esta función acepta la ruta del archivo como parámetro y devuelve una cadena con el contenido del archivo.

```Clojure
(slurp "mi-archivo.txt")
```

Si ejecutas este código, obtendrás el siguiente resultado:

```
=> "¡Hola mundo!"
```

## Profundizando

Aunque `spit` y `slurp` son funciones útiles para escribir y leer archivos de texto, también puedes utilizar el módulo `java.io` para realizar operaciones más avanzadas, como crear directorios y mover archivos. Puedes encontrar más información sobre esto en la documentación oficial de Clojure.

## Ver también

- [Documentación oficial de Clojure](https://clojure.org/)
- [Tutorial de Clojure de Codecademy](https://www.codecademy.com/learn/learn-clojure)
- [Guía de referencia de Clojure](https://clojure.org/reference)