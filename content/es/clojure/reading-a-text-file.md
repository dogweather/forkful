---
title:                "Clojure: Leyendo un archivo de texto"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué

La lectura de archivos de texto es una habilidad esencial para cualquier programador de Clojure. Sin esta habilidad, no podríamos acceder a datos externos y procesarlos en nuestro código. En este artículo, exploraremos cómo leer archivos de texto en Clojure y cómo puede ser útil en tus proyectos.

## Cómo hacerlo

Leer un archivo de texto en Clojure es muy sencillo. Primero, necesitamos utilizar la función `slurp`, que se encarga de leer el contenido del archivo y devolverlo como una cadena de texto. Por ejemplo, si queremos leer el contenido de un archivo llamado "texto.txt", podemos utilizar la siguiente línea de código:

```Clojure
(slurp "texto.txt") 
```

Esto nos devolvería una cadena con todo el contenido del archivo. Sin embargo, si queremos procesar los datos de forma más estructurada, podemos usar la librería `clojure.string` y su función `split` para dividir el texto en líneas o por algún otro separador. A continuación, un ejemplo de cómo leer un archivo de texto línea por línea:

```Clojure
(require '[clojure.string :as str])
(with-open [rdr (clojure.java.io/reader "texto.txt")] 
  (doseq [line (line-seq rdr)] 
    (println (str/split line #"\s+")))) 
```

Este código abrirá el archivo "texto.txt" y utilizará `line-seq` para recorrer todas las líneas del archivo. Luego, utilizando `split`, dividiremos cada línea por cualquier cantidad de espacios en blanco.

## Profundizando

Además de la función `slurp`, también podemos utilizar `clojure.java.io` para leer archivos de textos de forma más manual. Esta librería nos ofrece funciones para abrir y cerrar archivos, escribir en ellos, buscar rutas y demás. En particular, la función `reader` nos permite crear un lector de archivos, que podemos utilizar junto con `line-seq` para leer un archivo línea por línea.

## Ver también

Para más información sobre leer archivos de texto en Clojure, puedes revisar la documentación oficial [aquí](https://clojuredocs.org/clojure.java.io/reader). También puedes leer sobre otras funciones útiles como `print`, `println` y `with-open`. ¡Ahora estás listo para comenzar a leer archivos de textos en tus proyectos de Clojure!