---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:50.205821-07:00
description: "C\xF3mo hacerlo: El framework `Foundation` de Swift proporciona la clase\
  \ `Date`, lo que facilita obtener la fecha y hora actuales. Aqu\xED hay un ejemplo\
  \ b\xE1sico\u2026"
lastmod: '2024-03-13T22:44:59.426853-06:00'
model: gpt-4-0125-preview
summary: El framework `Foundation` de Swift proporciona la clase `Date`, lo que facilita
  obtener la fecha y hora actuales.
title: Obteniendo la fecha actual
weight: 29
---

## Cómo hacerlo:
El framework `Foundation` de Swift proporciona la clase `Date`, lo que facilita obtener la fecha y hora actuales. Aquí hay un ejemplo básico de cómo obtener la fecha actual:

```swift
import Foundation

let currentDate = Date()
print(currentDate)
```

Esto generará una salida similar a:

```
2023-04-12 07:46:23 +0000
```

El formato de salida sigue el estándar ISO 8601, utilizando la zona horaria UTC. Sin embargo, es posible que desees formatear esta fecha para fines de visualización. La clase `DateFormatter` de Swift viene al rescate:

```swift
let formatter = DateFormatter()
formatter.dateStyle = .long
formatter.timeStyle = .medium
let formattedDate = formatter.string(from: currentDate)
print(formattedDate)
```

La salida de muestra podría ser:

```
12 de abril de 2023 a las 10:46:23 AM
```

Nota que el formato de salida variará dependiendo de la localización del dispositivo que ejecute el código.

Para proyectos que requieran manipulación de fechas más compleja, muchos desarrolladores de Swift recurren a bibliotecas de terceros como `SwiftDate`. Así es cómo podrías usar `SwiftDate` para obtener la fecha actual en una zona horaria y formato específicos:

Primero, añade `SwiftDate` a tu proyecto usando SPM, CocoaPods, o Carthage. Luego:

```swift
import SwiftDate

let rome = Region(calendar: .gregorian, zone: .europeRome, locale: .current)
let currentDateInRome = DateInRegion(Date(), region: rome)
print(currentDateInRome.toFormat("yyyy-MM-dd HH:mm:ss"))
```

Esto podría generar:

```
2023-04-12 09:46:23
```

Usando `SwiftDate`, puedes manipular fácilmente fechas y horas para diferentes zonas horarias y localidades, simplificando tareas complejas de manejo de fechas en tus aplicaciones Swift.
