---
title:                "Comprobando si existe un directorio"
html_title:           "Clojure: Comprobando si existe un directorio"
simple_title:         "Comprobando si existe un directorio"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por qué
¿Alguna vez te has preguntado si un directorio existe antes de intentar manipularlo? Saber si un directorio existe puede ayudarte a evitar errores en tu código y asegurar que tus operaciones se realicen correctamente.

## Cómo hacerlo
La función `file-seq` en Clojure puede ser usada para verificar si un directorio existe. Esta función retorna una secuencia de los archivos y directorios dentro de un directorio específico. Si el directorio no existe, la secuencia será vacía.

```
```Clojure
(file-seq "/ruta/del/directorio")
; => ()
```
```

También puedes utilizar la función `file?` para verificar si un archivo existe. Esta función devuelve `true` si el directorio existe y `false` si no existe.

```
```Clojure
(file? "/ruta/al/archivo")
; => true
```
```

Finalmente, también puedes utilizar la función `exists?` para verificar si un directorio o archivo existe. Esta función devuelve `true` si el directorio o archivo existe y `false` si no existe.

```
```Clojure
(exists? "/ruta/al/archivo")
; => false
```
```

## Profundizando
La función `file-seq` también acepta un parámetro adicional `recursive`. Si se establece en `true`, la función también incluirá todos los subdirectorios y archivos dentro de ellos en la secuencia.

```
```Clojure
(file-seq "/ruta/del/directorio" true)
; => ("archivo1.txt" "archivo2.txt" "subdirectorio1/archivo3.txt" "subdirectorio2/ archivo4.txt")
```
```

La función `exists?` también acepta un segundo parámetro `follow-links`. Si se establece en `true`, la función seguirá y evaluará enlaces simbólicos, lo que puede ayudar a asegurar una verificación más precisa.

```
```Clojure
(exists? "/ruta/al/enlace" true)
; => true
```
```

## Ver también
- Documentación oficial de Clojure sobre las funciones de verificación de archivos y directorios: https://clojuredocs.org/clojure.core/file-seq
- Ejemplos de uso de `file?` y `exists?`: https://www.braveclojure.com/files-and-directories/