---
title:    "Clojure: Impresión de salida de depuración"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## ¿Por qué imprimir salida de depuración en Clojure?

La impresión de salida de depuración en Clojure es una práctica común para detectar y solucionar errores en el código. Al imprimir mensajes de depuración, podemos ver información detallada sobre el funcionamiento de nuestro programa, lo que nos ayuda a identificar dónde se producen los errores y cómo solucionarlos.

## Cómo imprimir salida de depuración en Clojure

Para imprimir salida de depuración en Clojure, podemos utilizar la función `println`. Esta función toma un argumento y lo imprime en la consola. En el siguiente ejemplo, imprimimos la variable `x`:

```Clojure
(def x "Hola Mundo")
(println x)

;; Salida:
Hola Mundo
```

También podemos utilizar la función `prn` para imprimir la representación en cadena de un objeto. Esta función imprime los elementos sin agregar espacios o nuevos saltos de línea. En el siguiente ejemplo, imprimimos una lista:

```Clojure
(def lista [1 2 3 4])
(prn lista)

;; Salida:
[1 2 3 4]
```

Otra opción es utilizar `pr-str`, que devuelve la representación en cadena de un objeto en lugar de imprimirlo. En el siguiente ejemplo, imprimimos una cadena con formato:

```Clojure
(def nombre "Juan")
(def edad 30)
(def ciudad "Madrid")

(println (pr-str "Hola, mi nombre es" nombre "tengo" edad "años y vivo en" ciudad))

;; Salida:
Hola, mi nombre es Juan tengo 30 años y vivo en Madrid
```

## Profundizando en la impresión de salida de depuración

Además de las funciones mencionadas anteriormente, también podemos utilizar `format` para imprimir cadenas con formato personalizado. Esta función toma una cadena de formato y una lista de argumentos. En el siguiente ejemplo, imprimimos un mensaje personalizado utilizando `format`:

```Clojure
(def nombre "Maria")
(format "¡Hola %s, bienvenida a nuestro programa!" nombre)

;; Salida:
¡Hola Maria, bienvenida a nuestro programa!
```

También podemos utilizar `prn-str` para imprimir y devolver la representación en cadena de un objeto. Esto es útil si queremos almacenar la salida de depuración en una variable para su posterior uso. En el siguiente ejemplo, almacenamos la representación en cadena de una lista en una variable llamada `salida`:

```Clojure
(def nombres ["Lucia" "Luis" "Sara"])
(def salida (prn-str nombres))

(println salida)

;; Salida:
"[\"Lucia\" \"Luis\" \"Sara\"]"
```

## Ver también

- [Documentación de Clojure sobre la función `println`](https://clojuredocs.org/clojure.core/println)
- [Documentación de Clojure sobre la función `prn`](https://clojuredocs.org/clojure.core/prn)
- [Documentación de Clojure sobre la función `pr-str`](https://clojuredocs.org/clojure.core/pr-str)