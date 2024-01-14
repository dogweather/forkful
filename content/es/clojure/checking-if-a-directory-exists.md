---
title:                "Clojure: Comprobando si existe un directorio"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por qué

Si estás programando en Clojure y necesitas saber si existe un directorio en tu sistema, es importante entender cómo hacerlo de manera eficiente y precisa.

## Cómo Hacerlo

Hay varias formas de verificar la existencia de un directorio en Clojure. Una forma sencilla es utilizar la función `file?` que devuelve `true` si la ruta especificada apunta a un archivo o directorio existente. Por ejemplo:

```Clojure
(file? "home/user/mis-documentos")
=> true
```

Sin embargo, esta forma no siempre es confiable ya que no toma en cuenta casos en los que el directorio especificado está vacío o no tiene permisos de lectura.

Otra opción es utilizar la función `dir?` que verifica si la ruta especificada apunta a un directorio existente y con contenido. Por ejemplo:

```Clojure
(dir? "home/user/mis-documentos")
=> true
```

Esta opción es más precisa ya que toma en cuenta tanto la existencia como el contenido del directorio.

Sin embargo, si lo que se desea es saber si un directorio específico existe, sin importar si tiene contenido o no, se puede utilizar la función `exists?`. Por ejemplo:

```Clojure
(exists? "home/user/mis-documentos")
=> true
```

Esta función devuelve `true` incluso en casos en los que el directorio existe pero está vacío. 

## Profundizando

Si quieres profundizar en el tema, puedes explorar diferentes posibilidades utilizando funciones como `files` y `dirs` que permiten obtener una lista de archivos y directorios en una ruta específica, respectivamente.

Además, también se puede utilizar la librería `clojure.java.io` que ofrece una gran variedad de funciones para manipular archivos y directorios de manera más detallada y precisa.

## Ver también

- [Documentación oficial de Clojure sobre archivos y directorios](https://clojure.github.io/clojure/clojure.java.io-api.html)
- [Ejemplos de código para trabajar con archivos y directorios en Clojure](https://github.com/clojure-cookbook/clojure-cookbook/blob/master/05_io/5-13_working-with-directories.asciidoc)