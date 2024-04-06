---
date: 2024-01-26 04:36:35.120892-07:00
description: "C\xF3mo hacerlo: Originalmente, los n\xFAmeros complejos fueron recibidos\
  \ con escepticismo, pero se han convertido en piezas centrales en varios campos\u2026"
lastmod: '2024-04-05T22:51:13.051801-06:00'
model: gpt-4-0125-preview
summary: "Originalmente, los n\xFAmeros complejos fueron recibidos con escepticismo,\
  \ pero se han convertido en piezas centrales en varios campos cient\xEDficos."
title: "Trabajando con n\xFAmeros complejos"
weight: 14
---

## Cómo hacerlo:
```Arduino
#include <Complex.h>

void setup() {
  Serial.begin(9600); // Iniciar comunicación serial
  
  Complex myComplex(2, 3); // Crear un número complejo 2 + 3i
  Complex anotherComplex(1, 1); // Crear otro número complejo 1 + 1i
  
  // Adición
  Complex result = myComplex + anotherComplex; 
  Serial.print("Adición: "); 
  result.print(); // Muestra 3 + 4i
  
  // Multiplicación
  result = myComplex * anotherComplex; 
  Serial.print("Multiplicación: ");
  result.print(); // Muestra -1 + 5i
}

void loop() {
  // No se usa en este ejemplo
}
```
Salida de muestra:
```
Adición: 3 + 4i
Multiplicación: -1 + 5i
```

## Profundización
Originalmente, los números complejos fueron recibidos con escepticismo, pero se han convertido en piezas centrales en varios campos científicos. Históricamente, se les reconoció por proporcionar soluciones a ecuaciones polinómicas que carecen de soluciones reales.

Arduino no incluye números complejos en su biblioteca estándar, pero puedes aprovechar bibliotecas como `Complex.h` para manejarlos. Internamente, estas bibliotecas definen una clase Complex, usando típicamente dos doubles para almacenar las partes real e imaginaria, y sobrecargan operadores para soportar aritmética.

Como alternativa, para aplicaciones que no necesitan inherentemente de la aritmética de números complejos, considera usar otras estrategias matemáticas o bibliotecas. Recuerda, sin embargo, que usar floats en lugar de números complejos podría simplificar demasiado algunos problemas.

## Ver También
- La biblioteca [Complex.h](https://github.com/RobTillaart/Complex) por Rob Tillaart.
- Una inmersión más profunda en la [matemática detrás de los números complejos](https://mathworld.wolfram.com/ComplexNumber.html).
