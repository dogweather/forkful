---
title:    "Clojure: Obteniendo la fecha actual"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por qué

En programación, es común necesitar la fecha actual para realizar ciertas tareas, como registrar eventos o establecer límites de tiempo. Afortunadamente, en Clojure, obtener la fecha actual es muy sencillo gracias a una función incorporada.

## Cómo hacerlo

```Clojure
(def fecha (java.util.Date.))
```

En el código anterior, estamos llamando a la función `java.util.Date.` para crear un objeto de fecha actual y asignarlo a la variable `fecha`.

```Clojure
(prn fecha)
```

Esta línea imprimirá la fecha actual en la consola en un formato detallado como `Mon Jul 27 13:59:42 EDT 2020`.

```Clojure
(.getTime fecha)
```

Si deseamos obtener la fecha en milisegundos desde la época, podemos llamar al método `getTime` en nuestro objeto `fecha`.

## Inmersión profunda

La clase `java.util.Date` en realidad representa un punto en el tiempo, no solo la fecha actual. Por lo tanto, si deseamos obtener solo la fecha actual sin la hora, podemos utilizar la clase `java.sql.Date` en su lugar.

```Clojure
(def solo-fecha (java.sql.Date.))
```

Esto creará un objeto de fecha actual sin la hora y nos permitirá hacer operaciones matemáticas con él.

```Clojure
(.setYear solo-fecha 2020)
```

Con el objeto `solo-fecha`, también podemos establecer valores específicos para el año, mes y día.

Para obtener más información sobre las funciones de fecha y hora en Clojure, puedes consultar la guía oficial de la documentación de Clojure en [este enlace](https://clojure.org/reference/java_interop#Time_and_Date_Manipulation).

## Ver también

- [Guía oficial de la documentación de Clojure](https://clojure.org/guides/learn/introduction)
- [Documentación de la clase `java.util.Date`](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
- [Documentación de la clase `java.sql.Date`](https://docs.oracle.com/javase/8/docs/api/java/sql/Date.html)