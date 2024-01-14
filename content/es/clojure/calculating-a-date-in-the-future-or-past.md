---
title:    "Clojure: Calculando una fecha en el futuro o pasado"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

# Por qué
Calcular fechas en el futuro o en el pasado puede ser una tarea útil en muchas situaciones, como la planificación de eventos o el manejo de fechas de vencimiento.

## Cómo hacerlo
Para calcular una fecha en el futuro o en el pasado en Clojure, se pueden seguir los siguientes pasos:

```Clojure
; Importar la librería java.time
(require '[java.time :as time])

; Definir una fecha base
(def fechaBase (time/now))

; Sumar o restar días, meses o años según sea necesario
(time/plus fechaBase (time/days 7)) ; Sumar 7 días
(time/minus fechaBase (time/months 1)) ; Restar 1 mes
(time/plus fechaBase (time/years 2)) ; Sumar 2 años

; Utilizar el método .format para mostrar la fecha en un formato deseado
(time/format fechaBase "dd MMMM uuuu") ; Mostrar la fecha en formato día mes año
```

El código anterior importa la librería `java.time` y utiliza sus métodos para sumar o restar días, meses o años a una fecha base y mostrar el resultado en un formato determinado. Este es solo un ejemplo básico, pero existen muchas otras formas de realizar cálculos con fechas en Clojure.

## Profundizando
Clojure tiene varias librerías disponibles para trabajar con fechas, como `java.time`, `clojure.java-time` y `clj-time`. Cada una ofrece diferentes funcionalidades y métodos para manejar fechas de manera eficiente. También es posible utilizar funciones de formato de fechas de la librería `java.text.SimpleDateFormat` para mostrar una fecha en un formato específico.

Otro aspecto importante a tener en cuenta al calcular fechas en el futuro o en el pasado es el manejo adecuado de zonas horarias y cambios de horario de verano. Estas librerías ofrecen métodos para manejar estas situaciones y asegurar la precisión de los cálculos.

# Ver también
- [Documentación oficial de la librería java.time](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Librería clj-time](https://github.com/clj-time/clj-time)
- [Librería clojure.java-time](https://github.com/dm3/clojure.java-time)