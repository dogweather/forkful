---
title:                "Comparando duas datas"
html_title:           "C: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que

Se você já se perguntou qual é a maneira correta de comparar duas datas em seu programa em C, este artigo é para você! Com este guia simples, você aprenderá como comparar duas datas de forma eficiente e sem complicações.

## Como fazer

Para comparar duas datas em C, você pode usar a função `difftime()` da biblioteca `time.h`. Esta função é projetada para calcular a diferença em segundos entre duas datas. Aqui está um exemplo de como usá-la:

```C
#include <stdio.h>
#include <time.h>

int main()
{
    // Criando duas estruturas de tempo contendo as datas de interesse
    struct tm data1 = {0};
    struct tm data2 = {0};

    // Definindo as datas (dia, mês, ano, hora, minuto, segundo) em cada estrutura de tempo
    data1.tm_mday = 21;
    data1.tm_mon = 7;
    data1.tm_year = 2021;

    data2.tm_mday = 22;
    data2.tm_mon = 7;
    data2.tm_year = 2021;

    // Calculando a diferença entre as datas em segundos
    double diferenca = difftime(mktime(&data2), mktime(&data1));

    // Exibindo a diferença em dias
    printf("A diferença entre as datas é de %f dias.", diferenca / (24 * 3600));

    return 0;
}
```

A saída deste programa será:

`A diferença entre as datas é de 1.000000 dias.`

## Aprofundando

A função `difftime()` retorna um valor `double` que representa a diferença em segundos. Porém, é possível convertê-lo para outras unidades de tempo, como minutos, horas ou dias. No exemplo acima, dividimos o resultado por 24 * 3600 (24 horas * 3600 segundos) para obter a diferença em dias.

Além disso, é importante lembrar que, antes de usar a função `difftime()`, as datas devem ser convertidas em estruturas de tempo usando a função `mktime()`. Essa função aceita uma estrutura de tempo e retorna o número de segundos desde o início da era Unix (01/01/1970).

A função `difftime()` pode ser útil para comparar duas datas e determinar qual é mais recente ou mais antiga. Por exemplo, se a diferença retornada for positiva, significa que a primeira data é mais recente do que a segunda.

## Veja também

- [Função difftime() em C](https://www.geeksforgeeks.org/difftime-function-in-c/)
- [Biblioteca time.h em C](https://www.tutorialspoint.com/c_standard_library/time_h.htm)