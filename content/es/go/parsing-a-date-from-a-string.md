---
title:                "Interpretando una fecha de una cadena de texto"
aliases:
- es/go/parsing-a-date-from-a-string.md
date:                  2024-02-03T18:00:01.144022-07:00
model:                 gpt-4-0125-preview
simple_title:         "Interpretando una fecha de una cadena de texto"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Parsear una fecha de un string en Go implica convertir la fecha representada como texto en un formato más utilizable (por ejemplo, `time.Time`). Los programadores realizan esta tarea para manejar datos de fecha y hora más precisamente en aplicaciones, especialmente cuando se trata de la entrada del usuario, APIs o sistemas de almacenamiento donde las fechas a menudo están representadas como strings.

## Cómo hacerlo:

Go ofrece un soporte robusto para parsear fechas y horas a través del paquete `time`. La clave está en entender el formato de fecha de referencia de Go: `Mon Jan 2 15:04:05 MST 2006`, que usas para decirle a Go cómo interpretar el string entrante. Aquí hay un ejemplo rápido para comenzar:

```go
package main

import (
	"fmt"
	"time"
)

func main() {
	// Ejemplo de string de fecha
	dateStr := "2023-04-12 14:45:00"
	
	// Definir el layout/formato del string de fecha de entrada
	// Este layout le dice a Go qué esperar un año, seguido de un mes, 
	// luego un día, hora, minuto y finalmente segundo
	layout := "2006-01-02 15:04:05"
	
	// Parsear el string de fecha de acuerdo al layout
	parsedDate, err := time.Parse(layout, dateStr)
	if err != nil {
		fmt.Println("Error al parsear la fecha:", err)
		return
	}
	
	// Mostrar la fecha parseada
	fmt.Println("Fecha Parseada:", parsedDate)
}
```

Cuando ejecutes este código, obtendrás:

```
Fecha Parseada: 2023-04-12 14:45:00 +0000 UTC
```

Nota cómo el string `layout` utiliza los valores de la fecha de referencia para especificar el formato del string de entrada. Ajusta el `layout` para que coincida con el formato de tus fechas de entrada.

## Análisis Detallado

El diseño del parseo de fechas y horas en Go es único, utilizando una fecha de referencia específica (`Mon Jan 2 15:04:05 MST 2006`). Esta aproximación, en lugar de usar especificadores de formato más convencionales (como `YYYY` para el año), fue elegida por su legibilidad y facilidad de uso, aprovechando un formato basado en ejemplos.

Aunque esto inicialmente puede parecer inusual para programadores acostumbrados a otros lenguajes, muchos lo encuentran más intuitivo después de un breve período de ajuste. Para aplicaciones que requieren manipulación de fechas más compleja o formatos no soportados directamente por el paquete `time` de Go, bibliotecas de terceros como `github.com/jinzhu/now` pueden ofrecer funcionalidad adicional. Sin embargo, para la mayoría de las aplicaciones estándar, las capacidades integradas de Go son robustas, eficaces y idiomáticas, encarnando la filosofía Go de simplicidad y claridad.
