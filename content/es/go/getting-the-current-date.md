---
title:                "Obteniendo la fecha actual"
date:                  2024-01-20T15:14:43.712352-07:00
html_title:           "Bash: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Qué & Por Qué?

Obtener la fecha actual en tu programa es básico pero crucial; es como preguntar, "¿qué día es hoy?" en el mundo digital. Los programadores usamos la fecha actual para registros, marcas de tiempo, y funcionalidades específicas centradas en el tiempo.

## Cómo Hacerlo:

Para obtener la fecha y hora actual en Go, usaremos el paquete `time`. Aquí un ejemplo:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	currentTime := time.Now()
	fmt.Println("Fecha y Hora Actual:", currentTime)
}
```

Output de muestra:

```
Fecha y Hora Actual: 2023-04-12 15:04:05.999999999 -0700 MST
```

Si solo quieres la fecha, sin la hora:

```Go
fmt.Println("Fecha Actual:", currentTime.Format("2006-01-02"))
```

Output de muestra:

```
Fecha Actual: 2023-04-12
```

## Deep Dive

La función `Now()` de Go retorna la fecha y hora actual obtenida del sistema operativo. 2006-01-02 15:04:05 es el formato de referencia en Go, cualquier otro formato se genera usando esta fecha de referencia, donde cada componente numeral tiene un significado único (por ej., el año se representa con 2006).

Historicamente, manejar fechas y horas ha sido complejo debido a zonas horarias, cambios de horario de verano, etc. Go simplifica esto con el paquete `time`, pero aún así uno debe ser consciente de la localidad para aplicaciones sensibles a la zona horaria.

Hay alternativas al paquete estándar como `github.com/jinzhu/now` para funcionalidades adicionales, aunque para la mayoría de casos, el paquete estándar es más que suficiente.

## Ver También

- Documentación oficial del paquete `time` en Go: [time package](https://pkg.go.dev/time)
- Librería para manipulación de fechas: [github.com/jinzhu/now](https://github.com/jinzhu/now)
