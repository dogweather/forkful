---
title:    "C: Gerando números aleatórios"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por que gerar números aleatórios é importante na programação?

Gerar números aleatórios é uma habilidade importante na programação porque permite que os programas possam tomar decisões ou criar resultados aleatórios. Isso é especialmente útil em jogos, simulações, criptografia e testes de software.

## Como gerar números aleatórios em C

Em C, existem diferentes maneiras de gerar números aleatórios. A primeira é usando a função `rand()` da biblioteca `stdlib.h`. Esta função retorna um número inteiro aleatório entre 0 e `RAND_MAX` (uma constante definida na biblioteca). Veja um exemplo:
```C
#include <stdio.h>
#include <stdlib.h>

int main() {
    // Gerar e imprimir dois números aleatórios
    int num1 = rand(); 
    int num2 = rand();
    printf("Número 1: %d\n", num1);
    printf("Número 2: %d\n", num2);
    return 0;
}
```
Exemplo de saída:
```
Número 1: 15127
Número 2: 6271
```
Outra forma é usando a função `srand()` para definir uma "semente" para a função `rand()`. Isso garante que os números gerados sejam diferentes a cada execução do programa. Veja um exemplo:
```C
#include <stdio.h>
#include <stdlib.h>
#include <time.h> // Biblioteca para usar a função time()

int main() {
    srand(time(0)); // Definir "semente" como o tempo atual
    // Gerar e imprimir dois números aleatórios
    int num1 = rand(); 
    int num2 = rand();
    printf("Número 1: %d\n", num1);
    printf("Número 2: %d\n", num2);
    return 0;
}
```
Exemplo de saída:
```
Número 1: 2078806323
Número 2: 56873282
```
É importante notar que os números gerados usando a função `rand()` não são realmente aleatórios, mas seguindo um padrão previsível. Se for necessário uma maior aleatoriedade, pode-se usar a função `random()` da biblioteca `unistd.h` ou as funções da biblioteca `openssl` para criptografia.

## Explorando mais sobre a geração de números aleatórios

A geração de números aleatórios é um assunto complexo e pode ser aprofundado em diferentes áreas, como estatística, probabilidade, criptografia e algoritmos. Se você quer saber mais sobre o tema, veja alguns links interessantes:

- [Tutorial de como usar a função `rand()` em C](https://www.escolalinux.com.br/func-o-de-gera-o-de-numeros-aleat-rios-rand-em-c-p_576)
- [Explicação sobre a "semente" e sua importância na geração de números aleatórios](https://www.educative.io/edpresso/uniform-random-number-generation-sswr6ztvi1)
- [Detalhes técnicos sobre a função `random()` em C](https://www.gnu.org/software/libc/manual/html_node/Random-Numbers.html)
- [Um estudo sobre geradores de números aleatórios no campo da criptografia](https://www.lambdageeks.com/advanced-methods-for-generating-cryptography-level-random-numbers-in-c/)

## Veja também

- [Como usar a biblioteca `openssl` para criptografias em C](https://www.openssl.org/docs/manmaster/man3/openssl.html)
- [Explicação sobre probabilidade e estatística para entender melhor a geração de números aleatórios](https://www.khanacademy.org/math/probability/probability-and-combinatorics-topic)