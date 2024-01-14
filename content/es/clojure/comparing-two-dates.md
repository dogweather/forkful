---
title:                "Clojure: Comparando dos fechas"
simple_title:         "Comparando dos fechas"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por qué

Hay muchas situaciones en las que puede ser útil comparar dos fechas en un programa en Clojure. Por ejemplo, puede necesitar verificar si una fecha es mayor que otra, si ambas fechas caen en el mismo día o mes, o si hay una diferencia específica de tiempo entre ellas. Afortunadamente, Clojure tiene funciones integradas que facilitan la comparación de fechas.

## Cómo

### Comparar si una fecha es mayor o menor que otra

Para comparar dos fechas en Clojure, utilizamos la función `>` para verificar si la primera fecha es mayor que la segunda y la función `<` para verificar si la primera fecha es menor que la segunda. Por ejemplo:

```Clojure
(> (java.util.Date. 2021 3 15) (java.util.Date. 2021 3 10))
;; muestra true, ya que 15 de marzo es mayor que 10 de marzo

(< (java.util.Date. 2021 3 5) (java.util.Date. 2021 3 10))
;; muestra true, ya que 5 de marzo es menor que 10 de marzo
```

### Verificar si dos fechas caen en el mismo día o mes

Para verificar si dos fechas caen en el mismo día o mes, podemos utilizar las funciones `=` y `month`. La función `=` compara si los dos argumentos son iguales, mientras que `month` nos da como resultado el mes correspondiente al argumento que le pasamos. Por ejemplo:

```Clojure
(= (month (java.util.Date. 2021 3 15)) (month (java.util.Date. 2021 3 20)))
;; muestra true, ya que ambos corresponden al mes de marzo

(= (month (java.util.Date. 2021 3 15)) (month (java.util.Date. 2021 4 15)))
;; muestra false, ya que uno es marzo y el otro es abril
```

### Obtener la diferencia de tiempo entre dos fechas

Podemos utilizar la función `days` para obtener la diferencia en días entre dos fechas. Esta función toma como argumentos dos fechas y devuelve un número entero que representa la diferencia en días entre ellas. Por ejemplo:

```Clojure
(days (java.util.Date. 2021 3 15) (java.util.Date. 2021 3 25))
;; muestra 10, ya que hay 10 días de diferencia entre el 15 y el 25 de marzo
```

## Profundizando

Además de las funciones mencionadas anteriormente, Clojure también ofrece otras funciones para trabajar con fechas, como `before?`, `after?`, `<=` y `>=`. Estas funciones proporcionan una forma más precisa de comparar fechas teniendo en cuenta aspectos como la hora y los milisegundos.

Es importante tener en cuenta que las fechas en Clojure son inmutables, por lo que cualquier función de comparación siempre devolverá un nuevo objeto `Date` y no modificará el objeto original.

## Ver también

- Documentación oficial sobre fecha y hora en Clojure: https://clojure.org/reference/java_interop#date_and_time
- Tutorial sobre uso de funciones de fecha en Clojure: https://www.braveclojure.com/core-functions-in-detail/#Date_and_Time_Functions