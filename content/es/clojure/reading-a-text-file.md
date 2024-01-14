---
title:    "Clojure: Leyendo un archivo de texto"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por Qué

¿Te has preguntado alguna vez cómo se pueden leer y procesar archivos de texto en el lenguaje de programación Clojure? En este post, te mostraremos cómo hacerlo de manera sencilla y eficiente.

##Cómo Hacerlo

La lectura de archivos de texto en Clojure se puede hacer con la función `slurp`. Esta función toma como argumento el nombre de un archivo de texto y devuelve una cadena de caracteres con todo su contenido. Por ejemplo:

```Clojure
(slurp "archivo.txt")
```

Esto devolverá una cadena con el contenido completo del archivo "archivo.txt".

Para poder procesar la información del archivo, es posible utilizar funciones de manipulación de cadenas, como `split`, `join` y `substring`. Estas funciones te permitirán separar la información en diferentes partes y realizar operaciones específicas con ella.

Por ejemplo, si queremos obtener cada línea del archivo en una lista, podemos utilizar la función `split` de la siguiente manera:

```Clojure
(split (slurp "archivo.txt") #"\n")
```

Esto devolverá una lista con todas las líneas del archivo.

## Deep Dive

Además de la función `slurp`, Clojure también cuenta con la función `clojure.java.io/reader` que permite leer y procesar archivos de manera más avanzada. Esta función toma como argumento un objeto del tipo `java.io.Reader` que se puede crear utilizando la función `clojure.java.io/reader` y el nombre del archivo. 

Por ejemplo, podemos obtener cada línea del archivo utilizando la función `clojure.java.io/reader` de la siguiente manera:

```Clojure
(with-open [reader (clojure.java.io/reader "archivo.txt")]
    (doseq [line (line-seq reader)]
        (println line)))
```

Esto imprimirá cada línea del archivo en la consola.

## See Also

Para obtener más información sobre el manejo de archivos de texto en Clojure, puedes consultar la documentación oficial: 

- [Función `slurp`](https://clojuredocs.org/clojure.core/slurp)
- [Funciones de manipulación de cadenas](https://clojuredocs.org/clojure.core/String)
- [Función `clojure.java.io/reader`](https://clojuredocs.org/clojure.java.io/reader)
- [Documentación oficial de Clojure](https://clojure.org/guides/io)