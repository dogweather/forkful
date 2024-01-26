---
title:                "Arredondamento de números"
date:                  2024-01-26T03:43:07.082173-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arredondamento de números"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/rounding-numbers.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Arredondar números consiste em eliminar os dígitos além de um certo ponto, enquanto, opcionalmente, ajusta-se o último dígito mantido. Programadores arredondam para reduzir a precisão quando valores exatos não são necessários, gerenciar erros de ponto flutuante ou preparar números para uma exibição amigável ao usuário.

## Como fazer:
Em C, você tipicamente usaria as funções `floor()`, `ceil()` ou `round()`. Aqui está um exemplo rápido:

```C
#include <stdio.h>
#include <math.h>

int main() {
    double num = 3.14159;
    double num_floor = floor(num);
    double num_ceil = ceil(num);
    double num_round = round(num);

    printf("Chão: %.2f\n", num_floor); // Chão: 3.00
    printf("Teto: %.2f\n", num_ceil);   // Teto: 4.00
    printf("Arredondamento: %.2f\n", num_round); // Arredondamento: 3.00
    return 0;
}
```

Para mais controle, como arredondar para um ponto específico, você multiplica, arredonda e divide:

```C
double arredondarParaPosicao(double num, int posicao) {
    double escala = pow(10.0, posicao);
    return round(num * escala) / escala;
}

// ...

double num = 3.14159;
double num_arredondado = arredondarParaPosicao(num, 2);
printf("Arredondado para 2 casas decimais: %.2f\n", num_arredondado); // Arredondado para 2 casas decimais: 3.14
```

## Mergulho Profundo
Antigamente, arredondar frequentemente significava um processo manual—uma tarefa pesada com apenas caneta e papel. Com a computação, automatizamos isso, mas a aritmética de ponto flutuante trouxe nuances devido à sua natureza binária, onde alguns números não podem ser representados exatamente.

Alternativas ao arredondamento padrão incluem a truncagem (simplesmente descartando dígitos extras) ou o arredondamento do banqueiro, que arredonda para o número par mais próximo quando exatamente entre dois valores, reduzindo o viés em cálculos repetidos.

A implementação se torna complicada quando você precisa arredondar números de precisão arbitrária ou lidar com casos especiais como infinito, NaNs sinalizadores ou valores subnormais. As funções da biblioteca padrão em C lidam com o básico, mas se você precisa arredondar decimais de formas personalizadas, precisará de mais do que `math.h`.

## Veja Também
- Documentação de [`<math.h>`](https://en.cppreference.com/w/c/numeric/math)
- [Aritmética de ponto flutuante](https://pt.wikipedia.org/wiki/Aritm%C3%A9tica_de_ponto_flutuante)
- [As armadilhas da verificação de cálculos de ponto flutuante](https://dl.acm.org/doi/10.1145/1186736.1186737)