---
title:                "Cambiando una fecha a una cadena"
html_title:           "Go: Cambiando una fecha a una cadena"
simple_title:         "Cambiando una fecha a una cadena"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Qué & Por Qué?
Las fechas son una parte importante en la programación, pero a menudo es necesario convertirlas en formato de texto para que sean más legibles para los usuarios. Esto se conoce como "convertir una fecha en una cadena". Los programadores hacen esto para mostrar las fechas en un formato más claro y fácil de entender.

## Cómo:
Go proporciona una forma muy sencilla de convertir una fecha en una cadena. Simplemente usamos la función `Format()` del paquete `time`, especificando el formato de fecha deseado. Por ejemplo:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	t := time.Now()
	// Convertir la fecha actual en una cadena con el formato "15-01-2006"
	str := t.Format("15-01-2006")
	fmt.Println(str)
}
```

Este código producirá la siguiente salida: `13-11-2020`.

Podemos utilizar diferentes formatos según nuestras necesidades. Por ejemplo, si queremos mostrar la fecha y hora actual en formato de 24 horas con minutos y segundos, podemos usar el formato "15:04:05":

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	t := time.Now()
	// Convertir la fecha actual en una cadena con el formato "15:04:05"
	str := t.Format("15:04:05")
	fmt.Println(str)
}
```

La salida sería algo como esto: `16:23:45`.

## Profundizando:
Convertir fechas en cadenas ha sido un desafío para los programadores durante mucho tiempo. La forma en que se hace en Go es bastante única, ya que se basa en una convención de formato de fecha específica. Sin embargo, hay otras formas de hacerlo, como usar librerías externas como `strftime`.

Para aquellos interesados en los detalles de implementación, es importante destacar que Go utiliza el paquete `time` para manejar fechas y la función `Format()` utiliza un layout de referencia específico, que es `Jan 2, 2006 at 3:04pm`, para formatear la fecha según nuestras necesidades.

## Ver También:
Si deseas obtener más información sobre cómo manejar fechas en Go, puedes consultar la documentación oficial del paquete `time` en la página web de Go: https://golang.org/pkg/time/. También puedes explorar otras alternativas para trabajar con fechas en Go, como la librería `timeutils` en GitHub: https://github.com/jinzhu/now. ¡Diviértete convirtiendo fechas en cadenas en tus próximos proyectos de Go!