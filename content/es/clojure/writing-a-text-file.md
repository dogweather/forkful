---
title:    "Clojure: Escribiendo un archivo de texto"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## ¡Por qué escribir un archivo de texto!

Escribir un archivo de texto es una habilidad fundamental para cualquier programador. Permite almacenar y organizar datos de manera legible y estructurada para su uso en diferentes aplicaciones. En este artículo, aprenderemos cómo escribir un archivo de texto en Clojure y profundizaremos en el tema.

## Cómo hacerlo

Para escribir un archivo de texto en Clojure, primero debemos usar la función `with-open` para abrir un flujo de escritura. Dentro de la función, especificaremos la ruta y nombre del archivo que se creará. Luego, usaremos la función `write` para escribir el contenido del archivo y `close` para cerrar el flujo de escritura.

```Clojure
(with-open [writer (clojure.java.io/writer "mi-archivo.txt")]
  (write writer "¡Hola, mundo!")
  (close writer))
```
El código anterior creará un archivo llamado "mi-archivo.txt" con el contenido "¡Hola, mundo!" en la misma ubicación que su archivo de código Clojure.

También podemos usar la función `println` para imprimir líneas adicionales en nuestro archivo de texto.

```Clojure
(with-open [writer (clojure.java.io/writer "mi-archivo.txt")]
  (println writer "Esta es una línea adicional")
  (close writer))
```

## Profundizando

Si queremos escribir una cantidad más grande de datos en nuestro archivo de texto, podemos usar la librería `clojure.data.csv`. Esta librería nos permite escribir matrices de datos en formato CSV (valores separados por comas) en nuestro archivo de texto.

Primero, debemos requerir la librería en nuestro código.

```Clojure
(require '[clojure.data.csv :as csv])
```

Luego, definimos una matriz de datos y especificamos la ruta y nombre del archivo que vamos a crear.

```Clojure
(def datos [["Nombre" "Edad" "País"]
            ["María" "25" "México"]
            ["Juan" "30" "España"]])

(with-open [writer (clojure.java.io/writer "datos.csv")]
  (csv/write-csv writer datos)
  (close writer))
```

El código anterior creará un archivo llamado "datos.csv" con los datos de nuestra matriz en formato CSV, así:

```
Nombre,Edad,País
María,25,México
Juan,30,España
```

## Vea también

- [Documentación de Clojure para la función `write`](https://clojuredocs.org/clojure.core/write)
- [Guía de uso de la librería `clojure.data.csv`](https://github.com/clojure/data.csv#usage)