---
title:    "Clojure: Obteniendo la fecha actual"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Por Qué

Obtener la fecha actual puede ser una tarea útil en una variedad de aplicaciones, desde crear marcas de tiempo en bases de datos hasta mostrar la fecha actual en una interfaz de usuario. Aprender cómo hacerlo en Clojure puede ahorrar tiempo y esfuerzo en el desarrollo de software.

## Cómo

Para obtener la fecha actual en Clojure, podemos usar la función `java.util.Date` y luego formatearla en el formato deseado usando `java.text.SimpleDateFormat`. Aquí hay un ejemplo de código que muestra cómo obtener la fecha actual en el formato "dd/MM/yyyy":

```Clojure
(import 'java.util.Date)
(import 'java.text.SimpleDateFormat)

(def fecha-actual (Date.))
(def formato (SimpleDateFormat. "dd/MM/yyyy"))

(println (str "La fecha actual es: " (.format formato fecha-actual)))

```

El resultado de este código será algo como: "La fecha actual es: 03/09/2021". En este ejemplo, primero importamos las clases `Date` y `SimpleDateFormat` y luego creamos dos variables: `fecha-actual` para almacenar la fecha actual y `formato` para determinar el formato deseado. Finalmente, usamos la función `.format` para formatear la fecha en el formato deseado y imprimirlo en la consola.

## Deep Dive

Además del formato "dd/MM/yyyy", también podemos obtener la fecha actual en otros formatos como "yyyy-MM-dd" o "MM-dd-yyyy". También podemos obtener otros detalles de la fecha, como la zona horaria y la hora actual. Esto es posible utilizando la clase `java.util.Calendar` en lugar de `java.util.Date`. Aquí está un ejemplo de cómo obtener la hora actual en el formato "HH:mm:ss":

```Clojure
(import 'java.util.Calendar)
(import 'java.text.SimpleDateFormat)

(def fecha-actual (Calendar/getInstance))
(def formato (SimpleDateFormat. "HH:mm:ss"))

(println (str "La hora actual es: " (.format formato (.getTime fecha-actual))))

```

El resultado de este código sería algo como: "La hora actual es: 10:20:35". Como se puede ver, en lugar de `java.util.Date`, usamos la función `Calendar/getInstance` para obtener una instancia de la fecha y luego usamos la función `.getTime` para obtener la hora actual.

## Ver También

- Documentación oficial de Clojure sobre fechas y horas: https://clojuredocs.org/clojure.core/time
- Tutorial de Clojure sobre fechas y horas: https://www.tutorialspoint.com/clojure/clojure_date_time.htm 
- Librería `clj-time` para trabajar con fechas y horas en Clojure: https://github.com/clj-time/clj-time