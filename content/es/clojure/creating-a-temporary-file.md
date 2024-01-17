---
title:                "Creando un archivo temporal"
html_title:           "Clojure: Creando un archivo temporal"
simple_title:         "Creando un archivo temporal"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Crear un archivo temporal en Clojure es una forma de generar un archivo temporal que se elimina automáticamente después de su uso. Los programadores lo hacen para almacenar datos temporales necesarios para un proceso específico, sin tener que preocuparse por eliminarlo manualmente después.

## How to:

El siguiente código muestra cómo crear un archivo temporal en Clojure utilizando la librería ```clojure.java.io```:

```
(require '[clojure.java.io :as io])
(with-open [file (io/file "temp.txt")] 
  (io/write-string file "Este es un archivo temporal") 
  (println "Archivo temporal creado: " (.getName file)))
```

Este código creará un archivo temporal llamado "temp.txt" y escribirá la cadena "Este es un archivo temporal" en él. Luego, se imprimirá el nombre del archivo creado.

Si quieres especificar la ubicación del archivo o el prefijo del nombre, puedes hacerlo utilizando las opciones ```:file``` y ```:prefix```. Por ejemplo:

```
(with-open [file (io/file {:file "Carpeta/temp.txt" :prefix "temp"})]
  (io/write-string file "Este es un archivo temporal") 
  (println "Archivo temporal creado: " (.getName file)))
```

En este caso, se creará un archivo temporal llamado "temp.txt" en la carpeta "Carpeta".

## Deep Dive:

La técnica de crear un archivo temporal es común en programación y no se limita solo a Clojure. Además de utilizar la librería ```clojure.java.io```, también puedes utilizar la función ```File/createTempFile``` de la librería ```java.io``` si estás trabajando con Java.

Otra alternativa es utilizar la librería ```clojure.data/file``` de Clojure para crear un archivo temporal a partir de datos. Esta función automáticamente creará el archivo y escribirá los datos en él.

## See Also:

- Documentación oficial de ```clojure.java.io```: https://clojure.github.io/clojure/clojure.java.io-api.html
- Documentación oficial de ```clojure.data/file```: https://clojuredocs.org/clojure.data/file
- Ejemplo de uso de ```clojure.data/file```: https://www.zilreader.com/blog/create-temp-file-with-clojure