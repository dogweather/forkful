---
title:                "Leyendo un archivo de texto"
html_title:           "Arduino: Leyendo un archivo de texto"
simple_title:         "Leyendo un archivo de texto"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Leer un archivo de texto en programación significa extraer datos de un archivo. Los programadores lo hacen para manipular, analizar y usar esos datos.

## ¿Cómo hacerlo?
Para leer un archivo de texto en Clojure, usamos la función `slurp`. Aquí está el código ejemplo:

```Clojure
(defn leer-archivo [nombre-archivo]
  (slurp nombre-archivo))

;; Usando la función para leer un archivo llamado 'ejemplo.txt'
(leer-archivo "ejemplo.txt")
```

Si el contenido de `ejemplo.txt` es `"¡Hola, Mundo!`, entonces la salida será: `"¡Hola, Mundo!`

## Análisis en Profundidad
Históricamente, la función `slurp` ha sido la forma estándar de leer archivos de texto en Clojure desde su creación. Sin embargo, existen alternativas como la función `line-seq` que lee el archivo línea por línea, útil cuando se manejan archivos grandes donde no deseas cargar todo el archivo en la memoria a la vez.

La implementación de `slurp` en Clojure no es muy complicada. Principalmente se apoya en las bibliotecas de E/S de Java para hacer el trabajo pesado. No requiere de cerrar explícitamente el archivo; `slurp` lo maneja automáticamente, lo cual es muy conveniente y limita la posibilidad de fugas de recursos.

## Ver también
Para más información sobre `slurp`, consulta la [documentación oficial](https://clojuredocs.org/clojure.core/slurp). Para entender mejor la lectura de archivos en general, puedes leer el [tutorial de Java sobre E/S](https://docs.oracle.com/javase/tutorial/essential/io/index.html). Y para más técnicas de programación en Clojure, el libro [The Joy of Clojure](https://www.manning.com/books/the-joy-of-clojure) es una excelente referencia.