---
title:                "Comparando dos fechas"
html_title:           "C#: Comparando dos fechas"
simple_title:         "Comparando dos fechas"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?

Comparar dos fechas significa determinar cuál es anterior o posterior. Los programadores suelen hacer esto por varias razones, como ordenar eventos cronológicamente, calcular intervalos de tiempo, entre otros.

## Cómo hacerlo:

Aquí te muestro cómo puedes comparar dos fechas en Go. Daremos uso a la biblioteca de tiempo `time` de Go.

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	fecha1 := time.Date(2021, 12, 10, 23, 20, 0, 0, time.UTC)
	fecha2 := time.Date(2022, 12, 10, 23, 20, 0, 0, time.UTC)

	if fecha1.Before(fecha2) {
		fmt.Println("Fecha1 es anterior a fecha2")
	} else if fecha1.After(fecha2) {
		fmt.Println("Fecha1 es posterior a fecha2")
	} else {
		fmt.Println("Ambas fechas son iguales")
	}
}
```

En la ejecución del código anterior, obtendrás: "Fecha1 es anterior a fecha2".

## Buceo Profundo:

Existen alternativas a la biblioteca de tiempo de Go. Puedes usar paquetes de terceros como `date`, que permite una manipulación más sencilla de las fechas.

Históricamente, la comparación de fechas ha sido un desafío debido a las diferentes zonas horarias y los cambios de hora. Go resuelve esta confusión a través del tipo `time.Time` que normalmente se utiliza en UTC.

Detallando la implementación: la función `Before` compara si la fecha1 es anterior a la fecha2. La función `After` lo contrario. Y si no son ni antes ni después, son exactamente iguales. 

## Ver También:

1. Documentación oficial sobre tiempos en Go: https://golang.org/pkg/time/
2. Guía práctica para la biblioteca de tiempo de Go: https://yourbasic.org/golang/format-parse-string-time-date-example/
3. Paquete `date` de terceros para Go: https://github.com/rickb777/date