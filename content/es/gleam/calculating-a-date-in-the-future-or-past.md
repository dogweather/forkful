---
title:    "Gleam: Calculando una fecha en el futuro o pasado"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Por qué

Calcular fechas en el futuro o en el pasado puede ser útil para planificar tareas, hacer seguimiento de eventos o simplemente para tener una idea de cuándo ocurrirá algo importante.

## Cómo hacerlo

Para calcular una fecha en el futuro o en el pasado en Gleam, debes utilizar la función `Date.add()`. Esta función acepta dos argumentos: una fecha y un número entero que representa la cantidad de días a añadir o restar. A continuación, se muestra un ejemplo de cómo usar esta función para calcular una fecha en el futuro:

```Gleam
let fecha_origen = Date.from_gregorian(2021, 10, 15)
let fecha_calculada = Date.add(fecha_origen, 7)
// fecha_calculada = 2021-10-22
```

En este ejemplo, se creó una variable `fecha_origen` con la fecha del 15 de octubre de 2021. Luego, utilizando la función `Date.from_gregorian()`, se convierte esta fecha al formato que utiliza Gleam. Finalmente, se llama a la función `Date.add()` para añadir 7 días a la fecha original, lo que resulta en la fecha del 22 de octubre de 2021.

Para calcular una fecha en el pasado, simplemente debes restar la cantidad de días que desees en lugar de sumarlos. En el siguiente ejemplo, se calcula una fecha 2 meses en el pasado:

```Gleam
let fecha_origen = Date.from_gregorian(2021, 10, 15)
let fecha_calculada = Date.add(fecha_origen, -60)
// fecha_calculada = 2021-08-15
```

Como puedes ver, la fecha resultante es el 15 de agosto de 2021.

## Profundizando

Si deseas profundizar en el cálculo de fechas en Gleam, es importante tener en cuenta que la función `Date.add()` también puede aceptar otros tipos de unidades de tiempo, como semanas, horas o minutos. Además, también se pueden realizar cálculos con fechas en otros formatos, como el calendario islámico o el juliano. Si necesitas más información sobre estos temas, puedes revisar la documentación oficial de Gleam o explorar los ejemplos en su repositorio de GitHub.

## Ver también

- [Documentación oficial de Gleam](https://gleam.run/documentation)
- [Repositorio de ejemplos de Gleam en GitHub](https://github.com/gleam-lang/gleam_examples)