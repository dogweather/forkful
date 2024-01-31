---
title:                "Refatoração"
date:                  2024-01-26T01:16:36.285986-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refatoração"

category:             "C"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/refactoring.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Refatoração é o processo de reestruturar o código de computador existente sem alterar seu comportamento externo. Programadores fazem isso para melhorar a legibilidade, reduzir a complexidade ou tornar o código mais mantível e escalável, o que pode economizar uma tonelada de tempo e dores de cabeça no futuro.

## Como fazer:
Vamos embelezar um código. Imagine que você tem uma função que calcula a média de inteiros em um array. À primeira vista, é um pouco confuso.

**Antes da Refatoração:**
```C
#include <stdio.h>

double calculateStuff(int numbers[], int size) {
    int sum = 0;
    double average;
    for(int i = 0; i < size; sum += numbers[i++]); // Somando na condição do loop for, ai!
    average = (double)sum / size;
    return average;
}

int main() {
    int array[] = {23, 42, 57, 16, 5, 102};
    int length = sizeof(array) / sizeof(array[0]);
    printf("Média: %f\n", calculateStuff(array, length));

    return 0;
}
```

**Após a Refatoração:**
```C
#include <stdio.h>

int calculateSum(const int numbers[], int size) {
    int sum = 0;
    for (int i = 0; i < size; ++i) {
        sum += numbers[i];
    }
    return sum;
}

double calculateAverage(const int numbers[], int size) {
    int sum = calculateSum(numbers, size);
    return (double)sum / size;
}

int main() {
    int array[] = {23, 42, 57, 16, 5, 102};
    int length = sizeof(array) / sizeof(array[0]);
    printf("Média: %f\n", calculateAverage(array, length));
    return 0;
}
```
Mesmo com esse exemplo simples, você pode ver como dividir a função torna o código mais limpo e mantível. Cada função agora tem uma única responsabilidade – um princípio chave na programação limpa.

## Mergulho Profundo
O termo "refatoração" foi popularizado no final dos anos 90, particularmente com a publicação do livro de Martin Fowler "Refatoração: Aperfeiçoando o Projeto de Código Existente". Refatorar não implica em consertar bugs ou adicionar novas funcionalidades, mas sim em melhorar a estrutura do código.

Existem muitas ferramentas de refatoração e IDEs (Ambientes de Desenvolvimento Integrados) bacanas que ajudam a automatizar o processo, como o CLion para C e C++, mas entender o que está acontecendo por baixo dos panos permanece crucial.

Alternativas à refatoração podem incluir reescrever o código do zero (arriscado e muitas vezes desnecessário) ou conviver com a dívida técnica (que pode ser mais custosa a longo prazo). Detalhes de implementação variam com base no projeto, mas refatorações comuns incluem renomear variáveis para maior clareza, quebrar funções grandes em menores e substituir números mágicos por constantes nomeadas.

Além disso, padrões como DRY (Don't Repeat Yourself - Não Se Repita) e princípios SOLID podem guiar sua jornada de refatoração, promovendo uma base de código que é mais fácil de testar, entender e colaborar.

## Veja Também
Para mergulhar mais fundo no mar da refatoração, dê uma olhada em:

- A página inicial de Martin Fowler: https://martinfowler.com/ com um tesouro de artigos e recursos sobre refatoração e design de software.
- Refactoring.com: https://refactoring.com/ fornece exemplos e catálogos de técnicas de refatoração.
- O livro "Refatoração": Considerado uma bíblia para refatoração, lê-lo proporciona uma visão completa da metodologia.
- "Código Limpo: Um Manual de Artesanato de Software Ágil" por Robert C. Martin, que discute escrever código que é fácil de entender e manter.
