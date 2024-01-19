---
title:                "Leyendo argumentos de la línea de comandos"
html_title:           "Bash: Leyendo argumentos de la línea de comandos"
simple_title:         "Leyendo argumentos de la línea de comandos"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

Leer argumentos de línea de comandos implica extraer y manipular información pasada al script que se está ejecutando a través de la terminal. Hacemos esto porque nos permite personalizar nuestros programas y hacerlos más versátiles.

## Cómo hacerlo:

En Clojure, se pueden leer argumentos de línea de comandos utilizando `*command-line-args*`. Mira este ejemplo:

```clojure
(doseq [arg *command-line-args*]
  (println arg))
```

Puedes pasar los argumentos en la terminal de esta manera:

```shell
$ java -cp clojure.jar clojure.main script.clj ARG1 ARG2 ARG3
```

`ARG1`, `ARG2`, y `ARG3` serán impresos uno por uno en la terminal debido al programa.

## Profundización:

(1) En el contexto histórico, leer argumentos de la línea de comandos ha sido una práctica común desde los primeros días de la programación, especialmente en el mundo UNIX. 

(2) Además de `*command-line-args*`, también puedes usar bibliotecas externas, como `tools.cli`, que ofrecen más opciones y flexibilidad para leer y analizar argumentos de línea de comandos.

(3) En cuanto a los detalles de implementación, `*command-line-args*` se trata de una variable global en Clojure y contiene una lista de cadenas. Cuando se inicia el programa, el runtime de Clojure llena esta variable con los argumentos que se pasaron por línea de comandos.

## Ver también:

Para aprender más sobre este tema, considera los siguientes recursos:

- Documentación oficial de Clojure: https://clojure.org/reference/vars#_dynamic_vars
- "Programación en Clojure": un libro por Alex Miller: http://shop.oreilly.com/product/9780596514983.do
- Biblioteca tools.cli en GitHub: https://github.com/clojure/tools.cli