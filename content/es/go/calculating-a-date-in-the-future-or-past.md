---
title:    "Go: Calculando una fecha en el futuro o pasado"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Porqué

Hay muchas situaciones en las que necesitamos saber la fecha en el futuro o en el pasado en nuestros programas, como por ejemplo, al crear una aplicación de calendario o al configurar recordatorios. Por eso es importante saber cómo calcular fechas en Go.

## Cómo

Calculando una fecha en el futuro o en el pasado en Go es sumamente fácil gracias a la librería incorporada de manejo de tiempo (time). Primero, importamos la librería con `import "time"` y luego utilizamos la función `AddDate()` para añadir o restar una cantidad especificada de años, meses y/o días a una fecha determinada. Veamos un ejemplo:

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    fecha := time.Now() //obtenemos la fecha de hoy
    fmt.Println("Fecha de hoy:", fecha.Format("02/01/2006"))
    
    fecha_futura := fecha.AddDate(2, 0, 0) //añadimos dos años a la fecha actual
    fmt.Println("Fecha en dos años:", fecha_futura.Format("02/01/2006"))
    
    fecha_pasada := fecha.AddDate(0, -3, 0) //restamos tres meses a la fecha actual
    fmt.Println("Fecha hace tres meses:", fecha_pasada.Format("02/01/2006"))
}
```

Este código nos dará la siguiente salida:

```
Fecha de hoy: 04/11/2021
Fecha en dos años: 04/11/2023
Fecha hace tres meses: 04/08/2021
```

## Deep Dive

Si queremos ir un poco más profundo en el manejo de fechas en Go, podemos utilizar la función `Parse()` para convertir una cadena de texto en una fecha y la función `Format()` para convertir una fecha en una cadena de texto con un formato específico. También existen librerías adicionales que nos permiten realizar operaciones más complejas, como cálculos entre diferentes zonas horarias o ajustes por horario de verano.

Es importante tener en cuenta que las fechas en Go se manejan en formato Unix, contando los segundos desde el 1 de enero de 1970 a las 00:00 UTC. Esto nos permite realizar operaciones más precisas y evita posibles errores de formato.

# See Also

- Documentación de la librería time en Go: https://golang.org/pkg/time/
- Tutorial de manejo de fechas en Go: https://golangbyexample.com/golang-date-time/
- Librería adicional para cálculos con zonas horarias: https://github.com/jinzhu/now