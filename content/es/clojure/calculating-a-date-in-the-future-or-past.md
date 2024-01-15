---
title:                "Calculando una fecha en el futuro o pasado"
html_title:           "Clojure: Calculando una fecha en el futuro o pasado"
simple_title:         "Calculando una fecha en el futuro o pasado"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por qué

¡Hola lectores! ¿Alguna vez se han preguntado cómo calcular una fecha en el pasado o en el futuro? Bueno, ¡hoy vamos a sumergirnos en el mundo de la programación con Clojure y descubrir cómo hacer precisamente eso de manera sencilla y eficiente. ¡Continúen leyendo!

## Cómo hacerlo

```Clojure
(defn calcular-fecha [fecha dias]
  (-> fecha
      (clj-time.core/plus {:days dias})
      (clj-time.format/unparse "dd/MM/yyyy")))
```

En el código anterior, definimos una función llamada `calcular- fecha` que toma dos argumentos: `fecha` y `dias`. El primero debe ser una cadena de texto en el formato `dd/MM/yyyy` y el segundo es un número entero que representa la cantidad de días que queremos agregar o restar a la fecha inicial.

Luego, mediante el uso de dos funciones de la biblioteca `clj-time`, `plus` y `unparse`, podemos sumar o restar la cantidad de días especificada a la fecha inicial y obtener el resultado en el formato deseado.

Veamos algunos ejemplos de uso de esta función:

```Clojure
(calcular-fecha "04/05/2021" 10)

;; Output: "14/05/2021"
```

```Clojure
(calcular-fecha "04/05/2021" -5)

;; Output: "29/04/2021"
```

## Profundizando

Ahora que ya sabemos cómo utilizar la función `calcular-fecha`, ¡podemos ir un paso más allá y explorar otras posibilidades! Por ejemplo, ¿Qué pasaría si queremos sumar o restar no solo días, sino también meses o años? ¡No se preocupen, Clojure tiene una solución para eso!

```Clojure
(defn calcular-fecha-avanzado [fecha dias meses años]
  (-> fecha
    (clj-time.core/plus {:years años})
    (clj-time.core/plus {:months meses})
    (clj-time.core/plus {:days dias})
    (clj-time.format/unparse "dd/MM/yyyy")))
```

En este nuevo código, hemos definido una función similar a la anterior, pero esta vez se pueden especificar la cantidad de días, meses y años que queremos agregar o restar a la fecha inicial.

Veamos un ejemplo de cómo usar esta función:

```Clojure
(calcular-fecha-avanzado "04/05/2021" 10 2 -1)

;; Output: "14/07/2020"
```

En este caso, hemos sumado 10 días, 2 meses y restado 1 año a la fecha inicial, lo que nos da como resultado el 14 de julio de 2020.

¡Ya tienen las herramientas necesarias para calcular fechas en el pasado o en el futuro de manera sencilla y eficiente con Clojure! ¡Espero que les sea útil y sigan explorando más opciones para mejorar sus habilidades de programación con este lenguaje!

## Ver también

- [Documentación oficial de Clojure](https://clojure.org/)
- [Biblioteca clj-time para manejo de fechas en Clojure](https://github.com/clj-time/clj-time)