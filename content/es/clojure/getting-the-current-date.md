---
title:                "Clojure: Obteniendo la fecha actual"
programming_language: "Clojure"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por qué

Obtener la fecha actual es una tarea común en la programación. Puede ser útil para registrar eventos, programar tareas o simplemente mostrar la fecha actual a un usuario.

## Cómo Hacerlo

En Clojure, podemos obtener la fecha actual utilizando la función `now` del paquete `java-time`. Primero, debemos importar el paquete:

```Clojure
(ns mi-app.core
    (:require [java-time :as jt]))
```

Luego, podemos llamar a la función `now` para obtener un objeto `LocalDateTime` que representa la fecha y hora actuales:

```Clojure
(def fecha-actual (jt/now))
```

Para mostrar la fecha actual en un formato legible, podemos utilizar la función `format` y especificar un patrón de formato. Por ejemplo, para mostrar la fecha en formato de día/mes/año:

```Clojure
(def fecha-formateada (jt/format fecha-actual "dd/MM/yyyy"))
```

El resultado será una cadena de texto con la fecha actual en el formato deseado.

## Profundizando

Además de la función `now`, el paquete `java-time` ofrece varias funciones y clases útiles para trabajar con fechas y horas. Por ejemplo, la función `plus` nos permite agregar una cantidad específica de tiempo a una fecha:

```Clojure
(def fecha-futura (jt/plus fecha-actual (jt/period 1 :day)))
```

La función `period` nos permite especificar el período que queremos agregar, en este caso 1 día. Luego, podemos formatear y mostrar esta nueva fecha al igual que lo hicimos antes.

Otra función útil es `with-zone`, que nos permite cambiar la zona horaria de una fecha determinada:

```Clojure
(def fecha-zona-horaria (jt/with-zone fecha-actual (jt/zone-offset "+05:00")))
```

Esto nos devuelve un nuevo objeto `LocalDateTime` con la misma fecha, pero en una zona horaria diferente.

## Ver También

- [Documentación de Java-Time en Clojure](https://github.com/dm3/clojure.java-time)
- [Tutorial de Clojure para principiantes](https://aprende.clojure.pro/)

¡Gracias por leer! Esperamos que esta guía te haya ayudado a comprender cómo obtener la fecha actual en tus proyectos de Clojure. ¡Feliz codificación!