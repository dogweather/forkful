---
title:                "Comparando dos fechas"
html_title:           "C++: Comparando dos fechas"
simple_title:         "Comparando dos fechas"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?

Comparar dos fechas significa determinar si una fecha es anterior, posterior o igual a la otra. Los programadores lo hacen para ordenar eventos, realizar conteos y condiciones para seguimientos o alertas.

## ¿Cómo?

Aquí, usamos la clase `Date` en Swift para crear y comparar fechas. Aquí está el código:

```Swift
let now = Date()
let pastDate = Date(timeIntervalSinceNow: -86400) // 1 día atrás

if now > pastDate {
    print("La fecha actual es después de la fecha pasada.")
} else if now < pastDate {
    print("La fecha actual es antes de la fecha pasada.")
} else {
    print("Las fechas son iguales.")
}
```

## Un Vistazo A Fondo

(1) **Contexto histórico**: Antes de Swift, Objective-C y Cocoa utilizaban la clase `NSDate` para manejar fechas. En Swift, esta ha sido reemplazada por `Date`.

(2) **Alternativas**: Existen muchas bibliotecas de terceros para manejar fechas, como `DateTools` y `SwiftDate`, que ofrecen más funcionalidad al comparar y manipular fechas.

(3) **Detalles de implementación**: En Swift, la comparación de fechas se implementa mediante la sobrecarga de los operadores `>`, `<`, `==`, `>=`, y `<=`. 

## Consulta Además

Para profundizar en el manejo de fechas con Swift, te recomiendo los siguientes enlaces:

- Documentación oficial de Swift: [Date](https://developer.apple.com/documentation/foundation/date)
- Stack Overflow: [Comparando fechas](https://stackoverflow.com/questions/24070450/how-to-get-the-difference-between-two-nsdates)
- NSHipster: [Date y DateComponents](https://nshipster.com/date/)