---
title:    "Gleam: Calculando una fecha en el futuro o pasado"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por qué
Calcular una fecha en el futuro o pasado puede ser una tarea útil en muchas aplicaciones. Puede ayudar a planificar eventos, recordar fechas importantes o incluso automatizar tareas.

## Cómo hacerlo
Para calcular una fecha en el futuro o pasado en Gleam, utilizamos la función `DateTime.add`. Esta función acepta un valor numérico, que representa la cantidad de días a añadir o restar, y una fecha base. Veamos un ejemplo:

```Gleam
import gleam/datetime

let fecha = datetime.new(2021, 9, 12) //12 de Septiembre de 2021
let fecha_futura = datetime.add(fecha, 7) //7 días en el futuro
let fecha_pasada = datetime.add(fecha, -14) //14 días en el pasado
```

El resultado de `fecha_futura` será el 19 de Septiembre de 2021, y el resultado de `fecha_pasada` será el 29 de Agosto de 2021. Para obtener más precisión y calcular también horas y minutos, podemos usar la función `DateTime.add_duration`:

```Gleam
let fecha = datetime.new(2021, 9, 12, 12, 30) //12 de Septiembre de 2021, 12:30 pm
let fecha_futura = datetime.add_duration(fecha, 2, 30, 0) //2 horas y 30 minutos en el futuro
let fecha_pasada = datetime.add_duration(fecha, -1, 15, 0) //1 hora y 15 minutos en el pasado
```

El resultado de `fecha_futura` será el 12 de Septiembre de 2021, 3:00 pm, y el resultado de `fecha_pasada` será el 12 de Septiembre de 2021, 11:15 am.

## Profundizando
Además de la función `DateTime.add`, Gleam también ofrece otras funciones útiles para trabajar con fechas:
- `DateTime.subtract`: Resta un período de tiempo a una fecha base.
- `DateTime.compare`: Compara dos fechas y devuelve un número positivo si la primera es más reciente que la segunda, o un número negativo si es más antigua. También devuelve 0 si ambas fechas son iguales.
- `DateTime.to_string`: Convierte una fecha en una cadena de texto, con formato personalizable.

Con estas funciones, podemos crear lógica adicional e implementar cálculos más complejos basados en fechas.

## Ver también
- [Documentación oficial de DateTime en Gleam](https://gleam.run/documentation/stdlib/datetime)
- [Guía de inicio rápido de Gleam](https://gleam.run/getting-started)
- [Ejemplos de proyectos en Gleam](https://gleam.run/examples)