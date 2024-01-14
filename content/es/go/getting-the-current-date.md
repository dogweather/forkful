---
title:                "Go: Obteniendo la fecha actual"
programming_language: "Go"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por qué obtener la fecha actual en Go

Puede parecer un concepto simple, pero obtener la fecha actual es una tarea importante en el desarrollo de cualquier programa. Ya sea para mostrar la fecha en una interfaz de usuario, registrar eventos en un sistema, o simplemente para realizar cálculos basados en la fecha actual, siempre es útil tener acceso a esta información.

## Cómo obtener la fecha actual en Go

En Go, obtener la fecha actual es bastante sencillo gracias al paquete `time` y su función `Now()` que permite recuperar la fecha y hora actual en un formato específico. A continuación, se muestra un ejemplo de código que imprime la fecha actual en formato `DD/MM/YYYY`:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	currentTime := time.Now()
	date := currentTime.Format("02/01/2006")
	fmt.Println("La fecha actual es:", date)
}
```

La salida de este programa sería: `La fecha actual es: 28/05/2021`. En este ejemplo, se utiliza el formato `02/01/2006` ya que en Go, el formato de fecha se define usando el formato de referencia `Mon Jan 2 15:04:05 MST 2006`.

Otro ejemplo sería obtener la hora actual en formato `HH:MM:SS`, para ello se puede utilizar el siguiente código:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	currentTime := time.Now()
	time := currentTime.Format("15:04:05")
	fmt.Println("La hora actual es:", time)
}
```

La salida sería: `La hora actual es: 14:28:35`.

## Profundizando en la obtención de la fecha actual en Go

Además de poder obtener la fecha y hora actuales, el paquete `time` cuenta con otras funciones útiles como `Parse()` que permite convertir una cadena de texto en formato de fecha a un objeto de tipo `time.Time`. También existe la función `Add()` que permite agregar o restar una cantidad de tiempo determinada a una fecha específica.

Una cosa importante a tener en cuenta es que la fecha y hora obtenida con `Now()` se basa en el huso horario del sistema, por lo que puede ser diferente a la fecha y hora actual en otras zonas horarias. Para obtener la fecha y hora en una zona horaria específica, se puede utilizar la función `LoadLocation()` para obtener una ubicación especificada y luego usar la función `In()` para convertir la fecha y hora al huso horario deseado.

## Ver también

- Documentación oficial de time en Go: https://golang.org/pkg/time/
- Tutorial sobre formateo de fecha en Go: https://programming.guide/go/format-parse-string-time-date-example.html
- Herramientas de timezone para Go: https://github.com/go-playground/locales