---
title:                "Comparando dos fechas"
html_title:           "Go: Comparando dos fechas"
simple_title:         "Comparando dos fechas"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

¿Qué y por qué?

Comparar dos fechas es un proceso común en la programación que consiste en determinar cuál de las dos fechas es más antigua o más reciente. Los programadores realizan esta tarea para ordenar fechas, realizar cálculos de tiempo y manejar lógica condicional.

¿Cómo hacerlo?

El lenguaje de programación Go ofrece una función integrada llamada "Before" que permite comparar dos fechas. Aquí hay un ejemplo de cómo usarla en un programa básico:

```
Go package main

import (
    "fmt"
    "time"
)

func main() {
    fecha1 := time.Date(2021, time.January, 1, 0, 0, 0, 0, time.UTC) // Primera fecha
    fecha2 := time.Date(2021, time.June, 1, 0, 0, 0, 0, time.UTC) // Segunda fecha

    if fecha1.Before(fecha2) { // Usando la función "Before" para comparar las dos fechas
        fmt.Println("La primera fecha es más antigua que la segunda")
    } else {
        fmt.Println("La segunda fecha es más antigua que la primera")
    }
}
```

La salida de este programa sería: "La primera fecha es más antigua que la segunda".

Profundizando

Esta función de comparación de fechas de Go se basa en el cálculo de segundos desde el 1 de enero de 1970 (conocido como "epoch") para cada una de las fechas. Si la fecha dada tiene una precisión más fina (por ejemplo, incluye horas y minutos), los segundos restantes se agregan al cálculo. Esto permite una comparación precisa incluso con fechas que tienen diferentes niveles de precisión.

Existe una alternativa a la función "Before" llamada "After", que realiza la comparación inversa. Además, los paquetes externos como "timeutil" proporcionan métodos adicionales para comparar fechas más específicamente, como ignorar el tiempo o las zonas horarias.

Véase también

- Documentación oficial de la función "Before" de Go: https://golang.org/pkg/time/#Time.Before
- Paquete "timeutil" para operaciones adicionales con fechas: https://github.com/golang/timeutil