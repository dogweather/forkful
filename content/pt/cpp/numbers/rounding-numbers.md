---
title:                "Arredondamento de números"
date:                  2024-01-26T03:43:13.205364-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arredondamento de números"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/rounding-numbers.md"
---

{{< edit_this_page >}}

## O que & Por quê?
Arredondar números significa ajustar um valor para o inteiro mais próximo ou uma precisão especificada. Desenvolvedores fazem isso para simplificar, conformar com restrições do mundo real, ou melhorar o desempenho ao descartar precisão excessiva.

## Como:
C++ oferece várias maneiras de arredondar números, como `floor()`, `ceil()`, e `round()`:

```C++
#include <iostream>
#include <cmath> // para funções de arredondamento

int main() {
    double num = 3.14;

    std::cout << "floor: " << std::floor(num) << "\n"; // Saída: floor: 3
    std::cout << "ceil: " << std::ceil(num) << "\n";   // Saída: ceil: 4
    std::cout << "round: " << std::round(num) << "\n"; // Saída: round: 3

    // Para precisão fixa, como arredondar para duas decimais:
    double precise_num = 3.146;
    double multiplicador = 100.0;
    double arredondado = std::round(precise_num * multiplicador) / multiplicador;

    std::cout << "arredondado para duas decimais: " << arredondado << "\n"; // Saída: arredondado para duas decimais: 3.15

    return 0;
}
```

## Mergulho Profundo
Antes do C++11, o arredondamento dependia de técnicas manuais ou bibliotecas não-padrão. Hoje, `<cmath>` fornece métodos robustos. `floor()` arredonda para baixo, `ceil()` arredonda para cima, enquanto `round()` vai para o inteiro mais próximo, lidando até com empates (casos de 0.5) arredondando para o número par.

Entender o comportamento dessas funções é crucial; por exemplo, números negativos podem confundir (`std::round(-2.5)` resulta em `-2.0`).

Alternativas? Converter para um int depois de adicionar 0.5 para números positivos era um truque clássico, mas erra com negativos e não é agnóstico de tipo. Bibliotecas como Boost podem oferecer abordagens mais refinadas, enquanto extensões de linguagem ou intrínsecos do compilador podem otimizar para hardware específico.

## Veja Também
- Referência C++ para `<cmath>`: https://en.cppreference.com/w/cpp/header/cmath
- Padrão IEEE para Aritmética de Pontos Flutuantes (IEEE 754): https://ieeexplore.ieee.org/document/4610935
- Biblioteca de Conversão Numérica Boost: https://www.boost.org/doc/libs/release/libs/numeric/conversion/
