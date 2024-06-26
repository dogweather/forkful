---
date: 2024-01-27 20:32:50.740527-07:00
description: "C\xF3mo hacerlo: Para generar n\xFAmeros aleatorios en C++, t\xEDpicamente\
  \ se hace uso del encabezado `<random>`, que fue introducido en C++11, ofreciendo\
  \ una\u2026"
lastmod: '2024-03-13T22:44:59.371296-06:00'
model: gpt-4-0125-preview
summary: "Para generar n\xFAmeros aleatorios en C++, t\xEDpicamente se hace uso del\
  \ encabezado `<random>`, que fue introducido en C++11, ofreciendo una amplia gama\
  \ de facilidades para generar n\xFAmeros aleatorios de varias distribuciones."
title: "Generaci\xF3n de n\xFAmeros aleatorios"
weight: 12
---

## Cómo hacerlo:
Para generar números aleatorios en C++, típicamente se hace uso del encabezado `<random>`, que fue introducido en C++11, ofreciendo una amplia gama de facilidades para generar números aleatorios de varias distribuciones.

```C++
#include <iostream>
#include <random>

int main() {
    // Inicializar un motor aleatorio
    std::random_device rd;  
    std::mt19937 gen(rd()); 

    // Definir el rango [0, 99] inclusivo
    std::uniform_int_distribution<> distrib(0, 99); 

    // Generar e imprimir 5 números aleatorios dentro del rango definido
    for(int n=0; n<5; ++n)
        std::cout << distrib(gen) << ' ';
    return 0;
}
```

Este ejemplo de código inicializa un generador de números aleatorios Mersenne Twister con una semilla de `std::random_device`. Luego define una distribución uniforme de enteros en el rango [0, 99] y finalmente imprime 5 números aleatorios de esta distribución.

La salida del ejemplo podría verse así, pero ten en cuenta que cada ejecución probablemente producirá resultados diferentes:

```
45 67 32 23 88
```

## Profundización:
Históricamente, la generación de números aleatorios en C++ dependía en gran medida de la función `rand()` y de la función `srand()` para la siembra, encontradas en el encabezado `<cstdlib>`. Sin embargo, este enfoque a menudo enfrentó críticas por su falta de uniformidad y previsibilidad en la distribución de los números generados.

La introducción del encabezado `<random>` en C++11 marcó una mejora significativa, ofreciendo un sistema sofisticado para producir números aleatorios. Las facilidades proporcionadas incluyen una variedad de motores (como `std::mt19937` para Mersenne Twister) y distribuciones (como `std::uniform_int_distribution` para la distribución uniforme de enteros) que se pueden combinar para satisfacer las necesidades específicas del programador, lo que lleva a un comportamiento más predecible, un mejor rendimiento y una mayor flexibilidad.

Si bien la biblioteca `<random>` es mucho mejor que el enfoque de `rand()` anterior, vale la pena notar que generar números verdaderamente aleatorios, especialmente para fines criptográficos, aún depende de consideraciones adicionales. Para aplicaciones criptográficas, se deben usar en su lugar bibliotecas diseñadas específicamente para la seguridad, que a menudo utilizan fuentes de entropía de hardware.
