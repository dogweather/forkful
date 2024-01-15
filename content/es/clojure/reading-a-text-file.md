---
title:                "Leyendo un archivo de texto."
html_title:           "Clojure: Leyendo un archivo de texto."
simple_title:         "Leyendo un archivo de texto."
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué

Si estás interesado en aprender Clojure o ya estás familiarizado con él, seguramente sabrás que es un lenguaje de programación dinámico, funcional y basado en la plataforma Java. Esto lo hace una excelente opción para trabajar con archivos de texto, ya que puede manejarlos de manera eficiente y con una sintaxis limpia. En este artículo, aprenderás cómo leer archivos de texto en Clojure y cómo aprovechar al máximo esta función.

## Cómo hacerlo

Leer un archivo de texto en Clojure es bastante sencillo. Primero, debemos abrir el archivo con la función `clojure.java.io/reader`, que toma como argumento la ruta al archivo que queremos leer. Luego, podemos utilizar la función `line-seq` para convertir el contenido del archivo en una secuencia de líneas. Aquí tienes un ejemplo de cómo hacerlo:

```Clojure
;; Creamos un reader para el archivo "texto.txt"
(def reader (clojure.java.io/reader "texto.txt"))

;; Convertimos el contenido en una secuencia de líneas
(def lineas (line-seq reader))

;; Imprimimos cada línea en la consola
(doseq [linea lineas]
  (println linea))
```

La salida del código anterior sería algo como esto:

```
Este es un ejemplo de archivo de texto.
Contiene varias líneas que podemos leer.
¡Genial, verdad?
```

También podemos utilizar funciones como `with-open` y `read-line` para leer y cerrar el archivo de manera automática:

```Clojure
;; Creamos un reader con with-open que se encargará de cerrar
;; el archivo automáticamente
(with-open [reader (clojure.java.io/reader "texto.txt")]
  ;; Utilizamos read-line para leer una línea del archivo
  (println (read-line reader)))
```

La salida de este código sería "Este es un ejemplo de archivo de texto." dado que read-line solo lee una línea a la vez.

## Inmersión profunda

Ahora que sabes cómo leer archivos de texto en Clojure, es importante tener en cuenta algunos aspectos adicionales. Por ejemplo, es posible que desees manejar errores si el archivo no existe o si no tienes permisos para leerlo. Para ello, puedes utilizar bloques `try` y `catch` para manejar estos casos.

También es importante recordar que Clojure utiliza la codificación UTF-8 por defecto, por lo que si estás trabajando con un archivo que utiliza una codificación diferente, deberás especificarlo para evitar problemas de caracteres.

Otra función útil es `slurp`, que permite leer todo el contenido de un archivo como una sola cadena. Sin embargo, debes tener cuidado al utilizar esta función para archivos grandes, ya que podría causar problemas de rendimiento.

## Ver también

Si quieres profundizar más en el manejo de archivos de texto en Clojure, puedes consultar los siguientes recursos:

- [Documentación oficial de Clojure sobre archivos de texto](https://clojure.org/reference/io)
- [Clojure Cookbook: Working with Files](https://clojure-cookbook.com/working-with-files/)
- [Introduction to Clojure: Reading and Writing Files](https://clojure.org/guides/learn/programming_files)

¡Espero que este artículo te haya sido útil y te ayude a manipular archivos de texto en tus proyectos de Clojure!