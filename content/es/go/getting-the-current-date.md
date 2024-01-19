---
title:                "Obteniendo la fecha actual"
html_title:           "C#: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## ¿Qué es y por qué?

Obtener la fecha actual significa que tu programa está accediendo a la fecha y hora actual del sistema. Los programadores hacen esto para marcar eventos, generar informes basados en el tiempo, o para programar tareas.

## Cómo hacerlo:

En Go, puedes obtener la fecha y hora actual con la función `Now` del paquete `time`. Aquí te muestro cómo:

```Go
package main
import "fmt"
import "time"

func main() {
    fmt.Println(time.Now())
}
```
Cuando ejecutas este programa, puedes esperar una salida similar a esta:

```Go
2009-11-10 23:00:00 +0000 UTC m=+0.000000001
```

## Profundizamos:

Históricamente, los lenguajes de programación han ofrecido diversas funciones para obtener la fecha y hora actual. En C, podrías usar `time(NULL)`. En Python, usarías `datetime.datetime.now()`. Sin embargo, Go hace un excelente trabajo manteniéndolo simple pero potente con la función `Now`.

Alternativamente, puedes usar la función `Unix` del paquete `time` para obtener la fecha y hora en formato Unix (segundos desde 1970):

```Go
fmt.Println(time.Now().Unix())
```

Al llamar a `Now`, estás obteniendo un objeto `Time` que incluye información más allá de sólo la fecha y hora. Este objeto incluye la zona horaria, y métodos para formatear la fecha y hora de la forma que quieras.

## Ver también:

Para obtener más detalles sobre el paquete `time` en Go, consulta [la documentación oficial de Go](https://golang.org/pkg/time/). Para aprender más sobre el manejo de fechas y horas en general, [w3schools tiene un excelente recurso](https://www.w3schools.com/go/go_date_time.asp).