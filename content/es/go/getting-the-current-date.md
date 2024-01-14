---
title:    "Go: Obteniendo la fecha actual"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Por qué
Obtener la fecha actual es una tarea común en muchas aplicaciones y programas. Ya sea para mostrar la fecha en una interfaz de usuario, realizar cálculos de tiempo o simplemente registrar la fecha y hora en un archivo, es importante saber cómo obtener la fecha actual en un programa Go.

## Cómo
La forma más sencilla de obtener la fecha actual en Go es utilizando la función `Now()` de la librería `time`. Ejemplo:

```Go
import (
    "fmt"
    "time"
)

func main() {
    fechaActual := time.Now()
    fmt.Println("La fecha actual es:", fechaActual)
}
```

Esto imprimirá la fecha y hora en el formato predeterminado de Go. Sin embargo, también puedes formatear la fecha a tu gusto utilizando el método `Format()` de la estructura `Time`. Por ejemplo:

```Go
fechaFormateada := time.Now().Format("02/01/2006")
fmt.Println("Hoy es:", fechaFormateada)
```

Este código imprimirá la fecha en el formato `DD/MM/AAAA`, que es común en muchos países de habla hispana.

## Profundizando
La función `Now()` de la librería `time` utiliza la ubicación del sistema para determinar la fecha y hora. Si quieres obtener la fecha y hora en una ubicación específica, puedes utilizar la función `Now()` junto con la función `In()` de la estructura `Time`.

Por ejemplo, si quieres obtener la fecha y hora en la ciudad de México, puedes hacerlo de la siguiente manera:

```Go
loc, _ := time.LoadLocation("America/Mexico_City")
fechaMexico := time.Now().In(loc)
```

Esto te dará la fecha y hora actual en la zona horaria de México.

## Ver también
- [Documentación oficial de la librería time en Go](https://golang.org/pkg/time/)
- [Tutorial de Go: Formateo de fechas y horas](https://golangbot.com/formatting-time-dates/)
- [Lista de zonas horarias soportadas por Go](https://golang.org/pkg/time/#LoadLocation)