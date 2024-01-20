---
title:                "Calculando una fecha en el futuro o pasado"
html_title:           "Swift: Calculando una fecha en el futuro o pasado"
simple_title:         "Calculando una fecha en el futuro o pasado"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Calcular una fecha futura o pasada es determinar una fecha específica a partir de una reducción o adición de un número específico de días, meses o años. Los programadores recurren a ello cuando se requiere programar eventos, recordatorios o tareas en las aplicaciones.

## Cómo Hacerlo:

Swift incorpora la clase `DateComponents` para este propósito. Podemos adicionar o restar días, meses, años, etc., a una fecha específica de esta manera:

```Swift
import Foundation

let fechaActual = Date()
var componentes = DateComponents()

// Adicionar 30 días a la fecha actual
componentes.day = 30
if let nuevaFecha = Calendar.current.date(byAdding: componentes, to: fechaActual) {
    print("La fecha después de 30 días será \(nuevaFecha)")
}
```

La salida del código anterior será la fecha después de 30 días desde el día actual.

## Inmersión Profunda

1. **Contexto Histórico**: Antiguamente, los programadores tenían que recurrir a funciones complicadas, manipulación directa de los tiempos de Unix y realizar tediosas conversiones para calcular fechas futuras o pasadas. Swift simplifica todo este proceso con la introducción de `DateComponents`.

2. **Alternativas**: Aunque `DateComponents` es la opción preferida en Swift, otras técnicas como las extensiones de `Date` también existen. Sin embargo, requieren más líneas de código y es más probable que introduzcan errores.

3. **Detalles de Implementación**: Calcular una fecha futura o pasada con `DateComponents` en su nivel más básico simplemente implica crear un objeto `DateComponents`, establecer su propiedad de día/mes/año y luego usar la función `date(byAdding:to:)` del calendario actual. Esta abstracción útil maneja todos los problemas de desbordamiento y ajustes de tiempo automáticamente.

## Ver También

- Documentación de Apple en `DateComponents`: [https://developer.apple.com/documentation/foundation/datecomponents](https://developer.apple.com/documentation/foundation/datecomponents) 
- Tutorial sobre el trabajo con fechas y horas en Swift: [https://www.hackingwithswift.com/articles/141/working-with-dates-and-times-in-swift](https://www.hackingwithswift.com/articles/141/working-with-dates-and-times-in-swift)