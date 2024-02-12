---
title:                "Arredondamento de números"
date:                  2024-02-03T18:07:39.600736-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arredondamento de números"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/rounding-numbers.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que e Por Quê?

Arredondar números é o processo de ajustar os dígitos de um número para reduzir sua precisão de acordo com certas regras, seja em direção ao número inteiro mais próximo ou a um número especificado de casas decimais. Programadores fazem isso por vários motivos, desde limitar a quantidade de armazenamento necessária, simplificar a saída para consumo do usuário ou garantir operações matemáticas precisas que são sensíveis a variações muito pequenas.

## Como Fazer:

Arredondar números em C pode ser realizado usando várias funções, mas a abordagem mais comum envolve as funções `floor()`, `ceil()` e `round()`. Estas funções fazem parte da biblioteca padrão de matemática, então você precisará incluir `math.h` em seu programa.

```c
#include <stdio.h>
#include <math.h>

int main() {
    double num = 9.527;

    // Usando floor() para arredondar para baixo
    double floorResult = floor(num);
    printf("floor(9.527) = %.0f\n", floorResult);

    // Usando ceil() para arredondar para cima
    double ceilResult = ceil(num);
    printf("ceil(9.527) = %.0f\n", ceilResult);

    // Usando round() para arredondar para o inteiro mais próximo
    double roundResult = round(num);
    printf("round(9.527) = %.0f\n", roundResult);

    // Arredondando para um número especificado de casas decimais envolve multiplicação e divisão
    double twoDecimalPlaces = round(num * 100) / 100;
    printf("Arredondando para duas casas decimais: %.2f\n", twoDecimalPlaces);

    return 0;
}
```

Saída:
```
floor(9.527) = 9
ceil(9.527) = 10
round(9.527) = 10
Arredondando para duas casas decimais: 9.53
```

## Aprofundamento

Arredondar números tem raízes históricas profundas na matemática e computação, integral tanto aos aspectos teóricos quanto aplicados. Em C, embora `floor()`, `ceil()` e `round()` ofereçam funcionalidade básica, a essência de arredondar floats para inteiros ou casas decimais específicas é mais matizada devido à representação binária dos números de ponto flutuante. Esta representação pode levar a resultados inesperados devido à forma como números que não podem ser precisamente representados em binário (como 0.1) são tratados.

Essas funções fazem parte da biblioteca padrão C, definidas em `<math.h>`. Ao arredondar números, especialmente para cálculos financeiros ou de engenharia precisos, deve-se considerar as implicações do uso de números de ponto flutuante binário. Alternativas às funções incorporadas em C para arredondamentos altamente precisos ou específicos para decimais podem incluir a implementação de funções de arredondamento personalizadas ou o uso de bibliotecas projetadas para aritmética de precisão arbitrária, como GMP ou MPFR, embora essas introduzam complexidade e dependências adicionais.

Na prática, escolher a abordagem correta para o arredondamento em C envolve equilibrar a necessidade de precisão, desempenho e praticidade, com uma compreensão aguçada dos requisitos específicos do domínio da aplicação que está sendo desenvolvida.
