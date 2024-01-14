---
title:    "Swift: Calculando una fecha en el futuro o pasado"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# ¿Por qué calcular una fecha en el futuro o en el pasado?

Si eres un programador de Swift, es muy probable que en algún momento hayas necesitado calcular una fecha en el futuro o en el pasado. Ya sea para mostrar información de eventos programados o para realizar cálculos de tiempo en tu aplicación, saber cómo calcular fechas es una habilidad muy útil. En este artículo, repasaremos cómo hacerlo en Swift de manera sencilla y eficiente.

## Cómo hacerlo

La forma más sencilla de calcular una fecha en el futuro o en el pasado es utilizando el método `addingTimeInterval` en la clase `Date`. Este método acepta un tiempo en segundos y devuelve una nueva fecha resultante de sumar o restar ese tiempo a la fecha original. Por ejemplo, si queremos obtener la fecha de mañana, podemos hacer lo siguiente:

```Swift
let tomorrow = Date().addingTimeInterval(86400) //86400 segundos en un día
print(tomorrow) //imprime la fecha de mañana
```

También es posible utilizar una función más específica, como `date(byAdding:value:to:)` que nos permite especificar el tipo de componente de tiempo que queremos sumar o restar (días, meses, años, etc).

## Profundizando

Ahora que hemos visto cómo calcular fechas de manera básica, es importante tener en cuenta ciertos detalles que pueden afectar los resultados. Por ejemplo, al sumar o restar tiempo a una fecha, el resultado puede variar dependiendo del calendario utilizado (gregoriano, hebreo, islámico, etc) y de la zona horaria en la que se encuentra el dispositivo. Es importante ser conscientes de estos factores y ajustarlos según sea necesario en nuestro código.

Además, es importante tener en cuenta que las fechas en Swift también contienen información sobre la hora, por lo que si solo queremos trabajar con fechas, es recomendable utilizar la clase `DateComponents` en lugar de `Date`. Esta clase nos permite especificar únicamente los componentes de fecha que nos interesan, ignorando la información de hora.

## Ver también

Si quieres profundizar aún más en el cálculo de fechas en Swift, aquí te dejamos algunos recursos útiles:

- [Documentación oficial de Apple sobre el manejo de fechas en Swift](https://developer.apple.com/documentation/foundation/date)
- [Un tutorial completo sobre el manejo de fechas en Swift](https://www.swiftbysundell.com/basics/dates/)
- [Una guía para trabajar con zonas horarias y calendarios en Swift](https://www.hackingwithswift.com/articles/117/calendars-and-timezones-using-calendar-and-datecomponents)