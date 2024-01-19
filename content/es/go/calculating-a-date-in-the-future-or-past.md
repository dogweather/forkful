---
title:                "Calculando una fecha en el futuro o pasado"
html_title:           "Go: Calculando una fecha en el futuro o pasado"
simple_title:         "Calculando una fecha en el futuro o pasado"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?

Calcular una fecha en el futuro o el pasado es determinar una fecha específica apartándose de una fecha inicial dada. Nosotros, como programadores, lo hacemos para actividades como temporización de eventos, rastreo de cambios y planificación de recordatorios.

## ¿Cómo hacerlo?

Aquí está un pedazo de código básico en Go que se usa para calcular fechas en el futuro/pasado.

```Go
paquete main 

importar (
    "fmt"
    "time"
)

func main() {
    hoy := time.Now()

    fmt.Println("Hoy es :", hoy)

    // Calcular una fecha 3 días desde ahora.
    futuro := hoy.Add(time.Hour * 24 * 3)

    fmt.Println("3 días desde ahora :", futuro)

    // Calcular una fecha 3 días antes.
    pasado := hoy.Add(-time.Hour * 24 * 3)

    fmt.Println("3 días atrás:", pasado)
}
```
Salida de muestra:

```Go
Hoy es: 2024-01-24 12:00:00 +0000 UTC
3 días desde ahora: 2024-01-27 12:00:00 +0000 UTC
3 días atrás: 2024-01-21 12:00:00 +0000 UTC
```

## Análisis en profundidad:

1. Contexto histórico: La capacidad de calcular fechas en el futuro o el pasado ha sido una necesidad en la programación desde sus primeros días. Originalmente se hizo a través de operaciones manualmente intensivas usando fechas Julianas o ticks del reloj. Sin embargo, con lenguajes modernos como Go, esta tarea se ha simplificado enormemente.

2. Alternativas: Además del método mostrado arriba, también puedes usar la función `AddDate` para operaciones más complejas. 

```Go
pasado := hoy.AddDate(0, 0, -3) // hacia atrás por 3 días
```
3. Detalles de implementación: La hora y la fecha en Go se manejan usando el paquete `time`. La funcionalidad para agregar o sustraer tiempo está incorporada en este paquete. La duración del tiempo (en este caso, un día) se especifica utilizando las constantes de tiempo disponibles en el paquete `time`. La función `Add` luego toma esta duración y devuelve la nueva fecha/hora.

## Ver también:

Para obtener más información, consulte la documentación oficial de Go sobre el paquete `time`: https://golang.org/pkg/time/ y otros recursos como:

- Método de adición de tiempo: https://golang.org/pkg/time/#Time.Add
- Método de adición de fecha: https://golang.org/pkg/time/#Time.AddDate