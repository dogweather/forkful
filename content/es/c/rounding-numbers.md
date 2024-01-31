---
title:                "Redondeo de números"
date:                  2024-01-26T03:42:49.443025-07:00
model:                 gpt-4-0125-preview
simple_title:         "Redondeo de números"

category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/rounding-numbers.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Redondear números es eliminar los dígitos más allá de cierto punto, ajustando opcionalmente el último dígito conservado. Los programadores redondean para reducir la precisión cuando los valores exactos no son necesarios, manejar errores de punto flotante o preparar números para una visualización amigable para el usuario.

## Cómo hacerlo:
En C, típicamente utilizarías las funciones `floor()`, `ceil()`, o `round()`. Aquí te mostramos rápidamente cómo:

```C
#include <stdio.h>
#include <math.h>

int main() {
    double num = 3.14159;
    double num_floor = floor(num);
    double num_ceil = ceil(num);
    double num_round = round(num);

    printf("Piso: %.2f\n", num_floor); // Piso: 3.00
    printf("Techo: %.2f\n", num_ceil);   // Techo: 4.00
    printf("Redondeo: %.2f\n", num_round); // Redondeo: 3.00
    return 0;
}
```

Para más control, como redondear a un lugar específico, multiplicas, redondeas y divides:

```C
double roundToPlace(double num, int place) {
    double escala = pow(10.0, place);
    return round(num * escala) / escala;
}

// ...

double num = 3.14159;
double num_redondeado = roundToPlace(num, 2);
printf("Redondeado a 2 lugares decimales: %.2f\n", num_redondeado); // Redondeado a 2 lugares decimales: 3.14
```

## Análisis profundo
En el pasado, redondear a menudo significaba un proceso manual, una tarea pesada con solo papel y lápiz. Con la computación, automatizamos esto, pero la aritmética de punto flotante trajo matices debido a su naturaleza binaria, donde algunos números no se pueden representar exactamente.

Las alternativas al redondeo estándar incluyen la truncación (simplemente descartar dígitos extra) o el redondeo de banqueros, que redondea al número par más cercano cuando está exactamente entre dos valores, reduciendo el sesgo en cálculos repetidos.

La implementación se complica cuando necesitas redondear números de precisión arbitraria o manejar casos especiales como infinito, NaNs señalando o valores subnormales. Las funciones de la biblioteca estándar de C manejan los conceptos básicos, pero si necesitas redondear decimales de maneras personalizadas, necesitarás algo más que `math.h`.

## Ver también
- [Documentación de `<math.h>`](https://en.cppreference.com/w/c/numeric/math)
- [Aritmética de punto flotante](https://es.wikipedia.org/wiki/Aritm%C3%A9tica_de_punto_flotante)
- [Las trampas de verificar los cálculos de punto flotante](https://dl.acm.org/doi/10.1145/1186736.1186737)
