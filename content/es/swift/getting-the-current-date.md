---
title:                "Obteniendo la fecha actual"
html_title:           "C#: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Obtener la fecha actual en la programación nos permite marcar cuando ocurren eventos específicos. Los programadores a menudo la utilizan para crear sellos de tiempo y realizar tareas relacionadas con la fecha y la hora.

## ¿Cómo se Hace?

Puedes obtener la fecha y hora actuales en Swift con el objeto `Date`. Aquí tienes un ejemplo:

```Swift
let ahora = Date()
print(ahora)
```

Corres este código, y veamos qué obtienes.

```Swift
2022-04-12 14:20:43 +0000
```

## Detalle Profundo

El objeto `Date` en Swift usa la hora Unix, que cuenta los segundos desde el 1 de enero de 1970. A diferencia de otras alternativas para obtener la fecha y hora actuales, `Date` tiene la ventaja de ser inmutable, lo que facilita su uso con múltiples hilos.

Para casos en los que las fechas y horas pueden requerir de un manejo más complejo, Swift ofrece la `Calendar` API. Esto puede ser útil si te encuentras con problemas de zonas horarias y fechas locales. Una limitación es que `Date` en Swift no tiene en cuenta las zonas horarias por sí mismo, simplemente representa un punto en el tiempo.

Aquí tienes un ejemplo de cómo puedes usar la `Calendar` API:

```Swift
let ahora = Date()
let calendario = Calendar.current
let hora = calendario.component(.hour, from: ahora)
let minuto = calendario.component(.minute, from: ahora)
print("Hora: \(hora), Minuto: \(minuto)")
```

Esto te dará la hora y el minuto actual del sistema.

## Ver También

Puedes aprender más sobre el manejo de fechas y horas en Swift en los siguientes enlaces:

- [Apple Developer Documentation - Date](https://developer.apple.com/documentation/foundation/date)
- [Apple Developer Documentation - Calendar](https://developer.apple.com/documentation/foundation/calendar)
- [NSHipster - Swift Dates, DateComponents, and Calendar](https://nshipster.com/datecomponents/)