---
title:    "Clojure: Convirtiendo una fecha en una cadena"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por qué

La conversión de una fecha a una cadena de caracteres es una parte importante del desarrollo de aplicaciones. Al hacerlo, nos permite mostrar la información de la fecha de forma legible para el usuario final y realizar operaciones con ella en nuestro código de manera más fácil y eficiente.

## Cómo hacerlo

```Clojure
;; Creamos una fecha utilizando la función `date`, especificando año, mes y día
(def fecha (java.util.Date. 2020 10 14))

;; Utilizamos la función `format` para convertir la fecha en una cadena con formato predefinido
;; En este caso, convertimos la fecha en una cadena con el formato "yyyy-MM-dd"
(format fecha "yyyy-MM-dd")
```

El resultado de la ejecución de este código será: "2020-10-14".
Podemos cambiar el formato de la cadena resultante cambiando los parámetros de la función `format`. Por ejemplo, si quisiéramos mostrar la fecha con el nombre del mes en vez del número, utilizaríamos el formato "yyyy-MMMM-dd" y el resultado sería: "2020-October-14".

```Clojure
;; Con la función `t` podemos obtener el objeto de tiempo correspondiente a la fecha
(def tiempo (t fecha))

;; Podemos utilizar la función `formatters` para obtener una lista con diferentes formatos disponibles
(formatters tiempo)
```

El resultado de la ejecución de este código será una lista con todos los formatos disponibles para la fecha y hora especificadas. Podemos utilizar esta lista para personalizar aún más la salida de la fecha en nuestra aplicación.

## Profundizando en la conversión de fechas a cadenas

Cuando hacemos uso de la función `format` para convertir una fecha a una cadena, en realidad estamos utilizando un formateador de fechas interno de Java llamado "SimpleDateFormat". Este formateador nos permite utilizar diferentes patrones para especificar el formato que queremos para nuestra cadena de fecha.

Además, también podemos utilizar la función `print-simple-date-format` para obtener más información sobre cómo se muestra una fecha específica en diferentes idiomas y para personalizar aún más los patrones de formato.

## Ver también

- [Java SimpleDateFormat Documentation](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
- [Clojure Time Library Documentation](https://clj-time.github.io/clj-time/0.13.0/api/clj-time.format.html)
- [Java Date and Time Tutorials](https://docs.oracle.com/javase/tutorial/datetime/index.html)