---
title:                "Clojure: Obteniendo la fecha actual."
simple_title:         "Obteniendo la fecha actual."
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por qué

Si estás programando en Clojure, es probable que en algún momento necesites obtener la fecha actual en tus programas. Ya sea para mostrarla en una interfaz de usuario, para realizar cálculos con fechas o simplemente para registrar la fecha en la que se realizó cierta acción, obtener la fecha actual es una tarea muy común en cualquier lenguaje de programación. En esta publicación, vamos a explorar cómo obtener la fecha actual en Clojure de una manera sencilla y eficiente.

## Cómo hacerlo

El lenguaje de programación Clojure tiene una función incorporada muy útil para obtener la fecha actual llamada `java.util.Date`. Usando esta función, podemos obtener la fecha actual en el siguiente formato:

```Clojure
(def fecha-actual (java.util.Date.))
```

Para mostrar la fecha en un formato más legible, podemos utilizar la función `format` de la biblioteca `clj-time`. Esta biblioteca proporciona una serie de funciones útiles para manipular y formatear fechas en Clojure. Para usarla, primero debemos agregar la dependencia correspondiente en nuestro proyecto:

```Clojure
[clj-time "0.14.1"]
```

Una vez agregada la dependencia, podemos formatear nuestra fecha de la siguiente manera:

```Clojure
(require '[clj-time.format :as fmt])
(println (fmt/unparse fmt/formatters "dd/mm/yyyy" fecha-actual))
```

Este código imprimirá la fecha en formato día/mes/año. Si queremos incluir la hora, podemos usar el formato "dd/mm/yyyy HH:mm:ss".

## Profundizando

En realidad, la función `java.util.Date` no devuelve una instancia de la clase `Date` sino que funciona como un puntero a un objeto mutable. Esto puede llevar a confusión al tratar de manipular y comparar fechas en nuestro programa. Es por eso que se recomienda el uso de la biblioteca `clj-time` para trabajar con fechas en Clojure.

Si queremos obtener la fecha actual en una zona horaria específica, podemos usar la función `now` de la biblioteca `clj-time` pasándole como parámetro un objeto `DateTimeZone` correspondiente a la zona deseada.

## Ver también

- Documentación oficial de `clj-time`: https://github.com/quantifind/clj-time
- Tutorial de Clojure: https://www.tutorialspoint.com/clojure/index.htm
- Artículo sobre Clojure en Wikipedia: https://es.wikipedia.org/wiki/Clojure