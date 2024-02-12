---
title:                "Trabajando con números complejos"
aliases: - /es/arduino/working-with-complex-numbers.md
date:                  2024-01-26T04:36:35.120892-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabajando con números complejos"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Los números complejos tienen una parte real y una parte imaginaria, escritos típicamente como `a + bi`. Son vitales para algunos proyectos de Arduino que requieren de matemáticas avanzadas, incluyendo el procesamiento de señales, ingeniería eléctrica, o cualquier otro dominio donde los fenómenos se modelan mejor en un plano.

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
