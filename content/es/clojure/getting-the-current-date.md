---
title:                "Obteniendo la fecha actual"
html_title:           "C#: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

---

## ¿Qué y Por Qué?

Obtener la fecha actual en programación es un proceso de recuperar el dato del tiempo presente en el sistema operativo. Los programadores a menudo lo requieren para registrar eventos, marcar operaciones con una estampa de tiempo en logs o manipular fechas para calcular la duración.

---

## Cómo Hacerlo:

En Clojure, la función de biblioteca `clojure.java-time` es tu amiga. La llamamos así:

```Clojure
(require '[java.time :as jt])
(jt/local-date)
```

Devolverá la fecha en este formato : `yyyy-mm-dd`. Aquí estás un ejemplo de salida:

```Clojure
2022-05-18
```

---

## Inmersión Profunda:

Historicamente, Clojure maneja fechas a través de las bibliotecas existentes de Java. En las primeras versiones se usaba `java.util.Date`, pero su diseño presentaba varias fallas y confusión, lo que llevó a la introducción del paquete `java.time` en Java 8 que solucionó muchos problemas.

Alternativamente, existen otras bibliotecas que puedes usar, como `clj-time` que es un wrapper alrededor de la librería Joda-Time de Java. Pero, se recomienda utilizar `java.time` ya que está basado en estándares ISO y es nativo en Java 8 y versiones superiores.

Detalles de Implementación: Cuando solicitas la fecha/hora actual en Clojure (o en Java), estás obteniendo el tiempo del sistema operativo del host. Esto significa que la fecha/hora que obtienes depende del reloj del sistema de tu ordenador.

---

## Ver También:

1. Documentación de `java.time`: https://clojure.github.io/clojure/clojure.java-time-api.html
2. El uso de `clj-time`: https://github.com/clj-time/clj-time
3. Librería `Joda-Time` para Java: https://www.joda.org/joda-time/