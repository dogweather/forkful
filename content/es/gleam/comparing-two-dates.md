---
title:                "Gleam: Comparando dos fechas"
programming_language: "Gleam"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

# ¿Por qué comparar dos fechas en Gleam?

 Comparar dos fechas es una tarea común en la programación, y en Gleam no es diferente. Al comparar dos fechas, podemos obtener información valiosa, como averiguar si una fecha es anterior o posterior a otra, o si tienen la misma fecha. En este artículo, exploraremos cómo realizar comparaciones de fechas en Gleam y profundizaremos en los detalles.

## Cómo hacerlo

Para comparar dos fechas en Gleam, podemos utilizar el módulo `Time.Date` que nos proporciona varias funciones útiles. Primero, necesitamos crear dos variables que contengan las fechas que queremos comparar, utilizando el formato `[año, mes, día]`. Por ejemplo, `fecha1 = [2019, 10, 17]` y `fecha2 = [2020, 5, 12]`. Luego, podemos utilizar la función `Date.compare` pasando como argumentos nuestras dos fechas para realizar la comparación. Esta función devuelve `Order.Less` si la primera fecha es anterior a la segunda, `Order.Equal` si son iguales y `Order.Greater` si la primera fecha es posterior a la segunda.

```Gleam
import gleam/time/date
fecha1 = [2019, 10, 17]
fecha2 = [2020, 5, 12]
comparacion = Time.Date.compare(fecha1, fecha2)
```

El valor de `comparacion` sería `Order.Less` en este caso, ya que la primera fecha es anterior a la segunda. Podemos utilizar un `if` statement para hacer algo con este resultado, por ejemplo, imprimir un mensaje en la consola.

```Gleam
if comparacion == Order.Less {
  Gleam.IO.print("La primera fecha es anterior a la segunda.")
}
```

También podemos utilizar la función `Date.eq` para verificar si dos fechas son iguales. Al igual que `Date.compare`, esta función devuelve `true` si las fechas son iguales y `false` en caso contrario.

## Profundizando en la comparación de fechas

Es importante tener en cuenta que cuando comparamos dos fechas en Gleam, estamos tratando con valores de tipo `Time.Date` y no con cadenas de texto. Esto significa que no podemos utilizar operadores de comparación como `>` o `<` directamente en nuestras fechas, ya que estos operadores solo funcionan con tipos de datos numéricos. En su lugar, debemos utilizar las funciones proporcionadas por el módulo `Time.Date`.

También es importante tener en cuenta que las fechas en Gleam solo pueden ser comparadas si están en el mismo calendario. Por ejemplo, no podemos comparar una fecha en el calendario gregoriano con una fecha en el calendario lunar. Si necesitamos comparar fechas de diferentes calendarios, podemos utilizar la función `Date.to_universal_calendar` para convertir la fecha a un formato universal antes de realizar la comparación.

En resumen, comparar dos fechas en Gleam puede brindarnos información valiosa sobre las mismas y nos permite tomar decisiones basadas en su relación. Pero es importante tener en cuenta los detalles y limitaciones mencionados anteriormente para asegurarnos de que nuestras comparaciones sean precisas.

## Ver también

- Documentación oficial del módulo `Time.Date` en Gleam: https://gleam.run/std/time.date.html
- Ejemplos de código de comparación de fechas en Gleam: https://gist.github.com/GleamHub/477dd516dce495bec4d37c7a059e6b7f
- Tutorial de Gleam en español: https://gleam.run/es/tour/