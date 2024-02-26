---
date: 2024-01-26 03:46:53.768510-07:00
description: "Redondear n\xFAmeros significa aproximar un valor num\xE9rico a una\
  \ precisi\xF3n espec\xEDfica, t\xEDpicamente para eliminar decimales no deseados.\
  \ Los programadores\u2026"
lastmod: '2024-02-25T18:49:55.881300-07:00'
model: gpt-4-0125-preview
summary: "Redondear n\xFAmeros significa aproximar un valor num\xE9rico a una precisi\xF3\
  n espec\xEDfica, t\xEDpicamente para eliminar decimales no deseados. Los programadores\u2026"
title: "Redondeo de n\xFAmeros"
---

{{< edit_this_page >}}

## Qué y Por Qué?

Redondear números significa aproximar un valor numérico a una precisión específica, típicamente para eliminar decimales no deseados. Los programadores redondean para gestionar la memoria, mejorar la legibilidad y cumplir con requisitos específicos del dominio como las restricciones de moneda.

## Cómo:

Swift provee varias maneras de redondear números. Aquí tienes un adelanto:

```Swift
let original = 3.14159

// Redondeo estándar
let standardRounded = round(original) // 3.0

// Redondeo a un lugar decimal específico
let decimalRounded = Double(round(original * 1000) / 1000) // 3.142

// Redondeo hacia abajo
let roundedDown = floor(original) // 3.0

// Redondeo hacia arriba
let roundedUp = ceil(original) // 4.0

print("Estándar: \(standardRounded), Decimal: \(decimalRounded), Abajo: \(roundedDown), Arriba: \(roundedUp)")
```

Salida: `Estándar: 3.0, Decimal: 3.142, Abajo: 3.0, Arriba: 4.0`

## Profundizando

Históricamente, el redondeo es un concepto matemático que precede a los ordenadores, esencial en el comercio y la ciencia. El marco de trabajo `Foundation` de Swift ofrece una funcionalidad de redondeo comprensiva:

- `round(_: )` es el buen viejo redondeo hacia arriba.
- `floor(_: )` y `ceil(_: )` manejan el redondeo direccional.
- `rounded(.up/.down/.toNearestOrAwayFromZero)` ofrece un control más fino con un enum de reglas de redondeo.

Ten en cuenta el tipo `Decimal` para cálculos financieros precisos, lo que evita errores de punto flotante. También, explora `NSDecimalNumber` para compatibilidad con Objective-C.

## Ver También

- Estándar IEEE para la Aritmética de Punto Flotante (IEEE 754): [IEEE 754](https://ieeexplore.ieee.org/document/4610935)
