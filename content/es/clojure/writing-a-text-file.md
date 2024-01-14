---
title:                "Clojure: Escribiendo un archivo de texto"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué

Escribir un archivo de texto es una habilidad importante en la programación en Clojure. Te permite almacenar y manipular datos de manera organizada y eficiente. Además, la escritura de archivos de texto a menudo es necesaria en la creación de aplicaciones y herramientas.

## Cómo hacerlo

La escritura de un archivo de texto en Clojure es una tarea sencilla gracias a las funciones integradas en el lenguaje. A continuación se muestra un ejemplo de código que escribe un archivo de texto y luego lo lee para mostrar su contenido:

```clojure
;; Definir el contenido del archivo
(def texto "Este es el contenido que se escribirá en el archivo.")

;; Crear el archivo en la carpeta actual
(spit "mi_archivo.txt" texto)

;; Leer el archivo y almacenar su contenido en una variable
(def contenido (slurp "mi_archivo.txt"))

;; Imprimir el contenido
(print contenido)
```

La salida de este código sería:

```
Este es el contenido que se escribirá en el archivo.
```

Como se puede ver, el archivo de texto se escribió de manera exitosa y su contenido se pudo leer y manipular fácilmente.

## Profundizando

Aunque escribir y leer archivos de texto en Clojure es una tarea simple, es importante conocer algunos detalles más avanzados. Por ejemplo, la función `spit` también puede tomar un tercer parámetro opcional que indica el tipo de codificación del archivo. Por defecto, usa la codificación `UTF-8`.

Otro detalle a considerar es que la función `slurp` devuelve una cadena de texto sin formato, por lo que puede ser necesaria una conversión de datos dependiendo de la estructura del archivo.

## Ver también

- [Documentación oficial sobre la función `spit`](https://clojuredocs.org/clojure.core/spit)
- [Documentación oficial sobre la función `slurp`](https://clojuredocs.org/clojure.core/slurp)
- [Ejemplos de escritura y lectura de archivos de texto en Clojure](https://clojure-examples.com/io/write-read-text-file/)