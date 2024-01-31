---
title:                "Calcular una fecha en el futuro o pasado"
date:                  2024-01-20T17:31:05.504041-07:00
model:                 gpt-4-1106-preview
simple_title:         "Calcular una fecha en el futuro o pasado"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Calcular fechas futuras o pasadas consiste en sumar o restar días, semanas, meses o años a una fecha dada. Los programadores lo hacen para manejar eventos, suscripciones, recordatorios y todo lo que dependa del tiempo.

## Cómo Hacerlo:
```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	// Fecha de hoy
	hoy := time.Now()
	fmt.Println("Hoy es:", hoy.Format("02-01-2006"))

	// Calcular una fecha en el futuro (10 días después)
	futuro := hoy.AddDate(0, 0, 10) 
	fmt.Println("Dentro de 10 días será:", futuro.Format("02-01-2006"))

	// Calcular una fecha en el pasado (30 días antes)
	pasado := hoy.AddDate(0, 0, -30) 
	fmt.Println("Hace 30 días fue:", pasado.Format("02-01-2006"))
}
```
Salida de muestra:
```
Hoy es: 30-03-2023
Dentro de 10 días será: 09-04-2023
Hace 30 días fue: 28-02-2023
```

## Profundización
Calcular fechas es esencial desde que las computadoras empezaron a planificar y organizar. En Go, se utiliza el paquete `time` para manejar fechas y horas. Alternativas a `time` incluyen librerías de terceros como `dateparse` para el análisis de fechas en formatos variados o `go-carbon` para la manipulación de fechas a la manera de Carbon en PHP. La manipulación de fechas es compleja debido a zonas horarias, años bisiestos y la variabilidad en la duración de meses. Go maneja esto internamente, por lo que no tienes que preocuparte por esos detalles, solo tienes que conocer los métodos proporcionados por el paquete `time`.

## Ver También
- Documentación oficial de Go sobre el paquete `time`: https://pkg.go.dev/time
- Go by Example: Time: https://gobyexample.com/time
- Biblioteca `dateparse` - https://github.com/araddon/dateparse
- Biblioteca `go-carbon` - https://github.com/golang-module/carbon
