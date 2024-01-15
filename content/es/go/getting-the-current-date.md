---
title:                "Obteniendo la fecha actual"
html_title:           "Go: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por qué 
¿Alguna vez has necesitado saber la fecha actual en tus programas en Go? ¡Entonces estás en el lugar correcto! En este artículo, aprenderás cómo obtener la fecha actual usando el lenguaje de programación Go.

## Cómo hacerlo 
Es muy sencillo obtener la fecha actual en Go. Solo necesitas importar el paquete "time" y utilizar la función "Now" para obtener la fecha actual.
```Go 
import "time"

currentDate := time.Now()
```

Si quieres obtener la fecha en un formato específico, puedes utilizar la función "Format" y especificar el formato deseado como argumento.
```Go 
import "time"

currentDate := time.Now()
formattedDate := currentDate.Format("02/01/2006") // Formato: DD/MM/YYYY
```

También puedes obtener otras propiedades como la hora, el día de la semana, o incluso la zona horaria.
```Go 
import "time"

currentDate := time.Now()
hour := currentDate.Hour()
weekday := currentDate.Weekday()
location := currentDate.Location()
```

## Profundizando
Ahora que sabes cómo obtener la fecha actual en Go, es importante entender cómo funciona esta función. La función "Now" utiliza el reloj del sistema para obtener la fecha y hora actual, por lo que siempre será precisa. Además, la función "Format" utiliza un sistema de códigos para especificar el formato deseado, por lo que puedes personalizarlo a tu gusto.

## Ver también
- Documentación oficial sobre el paquete "time" en Go: https://golang.org/pkg/time/
- Ejemplos de formato de fecha y hora en Go: https://yourbasic.org/golang/format-parse-string-time-date-example/ 
- Artículo sobre cómo utilizar paquetes en Go: https://medium.com/rungo/working-with-packages-in-go-3fd9d9ead4d1