---
title:                "Trabalhando com números complexos"
date:                  2024-01-26T04:37:35.536982-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabalhando com números complexos"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## O Quê e Por Quê?
Números complexos, uma mistura de partes real e imaginária (como 3 + 4i), são chave em cálculos avançados, como processamento de sinais ou resolução de certas equações. Programadores lidam com eles em aplicações pesadas de matemática onde números tradicionais não dão conta do recado.

## Como fazer:
C, desde o C99, tem um tipo nativo complexo e biblioteca. Aqui está como usar:

```C
#include <stdio.h>
#include <complex.h>

int main() {
    // Declara dois números complexos
    double complex z1 = 1.0 + 3.0 * I;
    double complex z2 = 2.0 - 2.0 * I;

    // Operações com números complexos
    double complex soma = z1 + z2;
    double complex mult = z1 * z2;

    // Imprimindo os resultados
    printf("Soma: %.1f + %.1fi\n", creal(soma), cimag(soma));
    printf("Produto: %.1f + %.1fi\n", creal(mult), cimag(mult));

    // Valor absoluto & ângulo de fase
    printf("Abs(z1): %f\n", cabs(z1));
    printf("Arg(z1): %f\n", carg(z1));

    return 0;
}
```

Saída de Exemplo:
```
Soma: 3.0 + 1.0i
Produto: 8.0 + 2.0i
Abs(z1): 3.162278
Arg(z1): 1.249046
```
## Aprofundamento
Números complexos remontam séculos, com raízes na álgebra do século 16. Avançando rápido, eles agora são fundamentais em muitas linguagens de programação, não apenas em C.

O padrão C99 introduziu `<complex.h>`, um cabeçalho que define macros, funções e o tipo de dados `complex`. Existem alternativas - como criar sua própria estrutura, mas por que reinventar a roda? A biblioteca padrão do C é otimizada e pronta para uso.

Apesar de sua potência, o suporte a complexos em C não está sem críticas. Pode ser menos intuitivo do que recursos similares em linguagens como Python, e lidar com casos de borda pode ser complicado. Mas para desempenho bruto, ainda é uma escolha sólida.

## Veja Também
- Documentação do Padrão C99 para `<complex.h>`: https://en.cppreference.com/w/c/numeric/complex
- Padrão IEEE para Aritmética de Ponto Flutuante (IEEE 754): https://ieeexplore.ieee.org/document/4610935
- Tutorial online para matemática de números complexos em C: https://www.tutorialspoint.com/complex-number-arithmetic-in-c-programming
