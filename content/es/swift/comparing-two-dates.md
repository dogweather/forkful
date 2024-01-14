---
title:    "Swift: Comparando dos fechas"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por qué
¿Te has preguntado alguna vez cómo comparar dos fechas en Swift? En esta publicación te explicaremos por qué a veces necesitas comparar fechas en tus programas y cómo hacerlo de manera sencilla.

## Cómo hacerlo
Para comparar dos fechas en Swift, podemos usar el operador `>` (mayor que), `<` (menor que) o `==` (igual que). Veamos algunos ejemplos:

```
let fecha1 = Date()
let fecha2 = Date(timeIntervalSinceNow: 86400)

if fecha1 > fecha2 {
    print("La fecha1 es mayor que la fecha2")
} else if fecha1 < fecha2 {
    print("La fecha1 es menor que la fecha2")
} else {
    print("Las fechas son iguales")
}

// Output: La fecha1 es menor que la fecha2
```

También podemos comparar solo las componentes de la fecha que nos interesen, como el día, el mes o el año. Por ejemplo:

```
let fecha1 = Date()
let fecha2 = Date(timeIntervalSinceNow: 86400)

let dia1 = Calendar.current.component(.day, from: fecha1)
let dia2 = Calendar.current.component(.day, from: fecha2)

if dia1 == dia2 {
    print("Ambas fechas son del mismo día")
}
```

## Deep Dive
Para comparar fechas de manera más precisa, podemos utilizar el método `compare` de la clase `Date`. Este método nos devuelve un objeto `ComparisonResult` que podemos utilizar en nuestras comparaciones. Por ejemplo:

```
let fecha1 = Date()
let fecha2 = Date(timeIntervalSinceNow: 86400)

let resultado = fecha1.compare(fecha2)

if resultado == .orderedDescending {
    print("La fecha1 es posterior a la fecha2")
} else if resultado == .orderedAscending {
    print("La fecha1 es anterior a la fecha2")
} else {
    print("Las fechas son iguales")
}

// Output: La fecha1 es anterior a la fecha2
```

Además, también existe la opción de utilizar la librería `Foundation` para comparar fechas con mayor precisión, teniendo en cuenta incluso variables como la zona horaria o la presencia de años bisiestos.

## Ver también
- [Documentación oficial de Swift sobre la clase Date](https://developer.apple.com/documentation/foundation/date)
- [Blog post sobre cómo utilizar la librería Foundation en Swift](https://www.raywenderlich.com/515-nsdate-tutorial-for-swift-how-to-work-with-dates-in-swift)
- [Ejemplos de comparaciones de fechas en Swift](https://www.hackingwithswift.com/example-code/language/how-to-compare-dates-in-swift)

*¡Esperamos que esta publicación te haya sido útil para comparar fechas en tus proyectos de Swift! ¡Hasta la próxima!*