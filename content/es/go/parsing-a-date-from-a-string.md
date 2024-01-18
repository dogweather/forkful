---
title:                "Extrayendo una fecha de una cadena"
html_title:           "Go: Extrayendo una fecha de una cadena"
simple_title:         "Extrayendo una fecha de una cadena"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
¿Alguna vez te has encontrado con una fecha escrita en un formato extraño y no sabes cómo convertirla a algo legible? Eso es donde entra en juego el "parsing" de una fecha a partir de una cadena de texto. Los programadores lo hacen para convertir una fecha escrita de manera no conveniente a un formato más legible y usable en sus programas.

## ¿Cómo hacerlo?
En Go, puedes usar la función time.Parse para convertir una cadena de texto en una fecha con el formato deseado. Por ejemplo, si tenemos una cadena de texto que sigue el formato "Año-Mes-Día", podemos usar la función de la siguiente manera:
```
fecha, _ := time.Parse("2006-01-02", "2021-05-28")
fmt.Println(fecha)
```
La salida será "2021-05-28 00:00:00 +0000 UTC". También puedes usar esta función para convertir una cadena con una zona horaria específica a una fecha en UTC (Tiempo Universal Coordinado).

## Profundizando
La necesidad de convertir fechas de un formato a otro existe desde los primeros días de la programación. Antes de time.Parse, los programadores tenían que escribir su propia función personalizada para convertir fechas, lo que a menudo resultaba en errores y código poco eficiente. Aunque hay otras alternativas como el paquete "dateparse" de Go, time.Parse sigue siendo la forma más sencilla y eficiente de hacer parsing de fechas en Go.

## Ver también
- [Paquete time en la documentación de Go](https://golang.org/pkg/time/)
- [Librería dateparse de Go](https://github.com/araddon/dateparse)
- [Artículo sobre el paquete time en el blog de Go](https://blog.golang.org/playground)