---
title:                "Clojure: Transformando una fecha en una cadena"
simple_title:         "Transformando una fecha en una cadena"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por qué
Convertir una fecha en una cadena de texto puede ser una tarea útil en muchas situaciones diferentes, como por ejemplo en la creación de informes o en la visualización de datos en una interfaz de usuario.

## Cómo hacerlo
Para convertir una fecha en una cadena de texto en Clojure, se puede utilizar la función `str` junto con la función `format` de la librería `clojure.java-time`. A continuación se muestra un ejemplo de cómo convertir la fecha actual en una cadena de texto en el formato "DD/MM/YYYY":

```Clojure
(require '[java-time :as jt])
(str (jt/local-date) "DD/MM/YYYY")
```
La salida de este código sería algo como "04/08/2021", dependiendo de la fecha actual en la que se ejecute.

## Profundizando
La función `format` también acepta un patrón de formato personalizado para la conversión de fechas. Por ejemplo, si se desea obtener la fecha en formato "DD de MMM de YYYY" (ej: 04 de ago. de 2021), se puede utilizar el patrón "dd 'de' MMMM 'de' uuuu" en la función `format`:

```Clojure
(require '[java-time :as jt])
(str (jt/local-date) (jt/format "dd 'de' MMMM 'de' uuuu"))
```
Además, es posible especificar un idioma específico para el formato de la fecha, por ejemplo, en español:

```Clojure
(require '[java-time :as jt])
(str (jt/local-date) (jt/format "dd 'de' MMMM 'de' uuuu" (jt/locales :es)))
```

## Ver también
- [Documentación de la librería clojure.java-time](https://clojure.github.io/java-time/)
- [Guía de formato de fechas en Java](https://docs.oracle.com/javase/tutorial/i18n/format/dateFormat.html)