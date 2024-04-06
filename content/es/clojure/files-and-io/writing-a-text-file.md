---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:22.856652-07:00
description: "C\xF3mo hacerlo: La funci\xF3n `spit` es la manera m\xE1s sencilla de\
  \ escribir texto en un archivo en Clojure. Toma dos argumentos: la ruta del archivo\
  \ y la cadena\u2026"
lastmod: '2024-03-13T22:44:58.676450-06:00'
model: gpt-4-0125-preview
summary: "La funci\xF3n `spit` es la manera m\xE1s sencilla de escribir texto en un\
  \ archivo en Clojure."
title: Escribiendo un archivo de texto
weight: 24
---

## Cómo hacerlo:


### Escribir texto en un archivo usando las funciones integradas de Clojure
La función `spit` es la manera más sencilla de escribir texto en un archivo en Clojure. Toma dos argumentos: la ruta del archivo y la cadena de texto a escribir. Si el archivo no existe, `spit` lo creará. Si existe, `spit` lo sobrescribirá.

```clojure
(spit "example.txt" "¡Hola, mundo!")
```

Para añadir texto a un archivo existente, puedes usar la función `spit` con la opción `:append`.

```clojure
(spit "example.txt" "\nVamos a agregar esta nueva línea." :append true)
```

Después de ejecutar estos fragmentos, "example.txt" contendrá:

```
¡Hola, mundo!
Vamos a agregar esta nueva línea.
```

### Usando bibliotecas de terceros
Aunque las capacidades integradas de Clojure son a menudo suficientes, la comunidad ha desarrollado bibliotecas robustas para tareas más complejas o específicas. Para I/O de archivos, una biblioteca popular es `clojure.java.io`, que proporciona un enfoque más similar a Java para el manejo de archivos.

Para usar `clojure.java.io` para escribir en un archivo, primero necesitas importarlo:

```clojure
(require '[clojure.java.io :as io])
```

Luego, puedes usar la función `writer` para obtener un objeto escritor, y la función `spit` (u otras como `print`, `println`) para escribir en el archivo:

```clojure
(with-open [w (io/writer "example_with_io.txt")]
  (.write w "Esto está escrito usando clojure.java.io"))
```

Esto creará (o sobrescribirá si ya existe) "example_with_io.txt" con el texto:

```
Esto está escrito usando clojure.java.io
```

Recuerda: `with-open` asegura que el archivo se cierre adecuadamente después de escribir, evitando potenciales fugas de recursos.
