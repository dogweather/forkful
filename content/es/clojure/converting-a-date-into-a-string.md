---
title:    "Clojure: Convirtiendo una fecha en una cadena"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

# Por qué

Converting a date into a string is a fundamental task in many programming languages, including Clojure. It allows us to represent dates in a human-readable format and use them in various operations, such as sorting and comparing. In this blog post, we will explore how to easily convert a date into a string using Clojure.

# Cómo hacerlo

En Clojure, podemos utilizar la función `str` para convertir una fecha en una cadena. Esta función toma un parámetro opcional que permite especificar un formato de fecha. Si no se proporciona un formato, por defecto se utilizará el formato `YYYY-MM-DD`.

```Clojure
(str "2021-09-15")
```
Output: "2021-09-15"

Si queremos especificar un formato diferente, podemos usar la función `clj-time.format/unparse`, que está incluida en la biblioteca `clj-time`.

```Clojure
(require '[clj-time.format :as time-format])
(time-format/unparse (time-format/formatters :date) (time "2021-09-15"))
```
Output: "Sep/15/2021"

Además, podemos convertir una fecha a una cadena con un formato personalizado utilizando la función `java.time.format.DateTimeFormatter` incorporada en Java.

```Clojure
(import '[java.time.format DateTimeFormatter])
(def formatter (DateTimeFormatter/ofPattern "dd/MM/YYYY"))
(.format (java.time.LocalDateTime/now) formatter)
```
Output: "15/09/2021"

# Inmersión profunda

Cuando convertimos una fecha en una cadena, es importante tener en cuenta que el resultado dependerá del formato utilizado y de la configuración regional del sistema.

Por ejemplo, si nuestro sistema está configurado en español, la fecha se mostrará en el formato DD/MM/YYYY por defecto, mientras que en un sistema configurado en inglés se mostrará en el formato MM/DD/YYYY.

Por otro lado, también podemos especificar opciones adicionales en el formato, como mostrar el día de la semana o la zona horaria. Para obtener más información sobre las opciones de formato disponibles, podemos consultar la documentación de la biblioteca `clj-time`.

# Ver también

- [Documentación de la función `str` en Clojure](https://clojuredocs.org/clojure.core/str)
- [Documentación de la biblioteca `clj-time`](https://clojure.github.io/clj-time/)
- [Documentación de `java.time.format.DateTimeFormatter` en Java](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)