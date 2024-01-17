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

## O que & Por quê?
Comparar duas datas é uma tarefa comum no mundo da programação. Isso ocorre porque, muitas vezes, precisamos verificar se uma data é anterior, posterior ou igual a outra, para tomar decisões lógicas no nosso código.

## Como fazer:
A linguagem C possui diversas funções para trabalhar com datas. Vejamos alguns exemplos de como comparar duas datas utilizando diferentes funções:
```
#include <stdio.h>
#include <time.h>

int main() {
    // Exemplo 1: Comparando dois objetos do tipo struct tm
    
    struct tm dt1 = { .tm_year=2021, .tm_mon=7, .tm_mday=29 };
    struct tm dt2 = { .tm_year=2021, .tm_mon=7, .tm_mday=30 };

    if (difftime(mktime(&dt1), mktime(&dt2)) > 0) {
        printf("A data 1 é posterior a data 2!\\n");
    } else if (difftime(mktime(&dt1), mktime(&dt2)) < 0) {
        printf("A data 1 é anterior a data 2!\\n");
    } else {
        printf("As datas são iguais!\\n");
    }

    // Exemplo 2: Comparando duas strings no formato "AAAA-MM-DD"

    char dt1[] = "2021-07-29";
    char dt2[] = "2021-07-30";

    if (strcmp(dt1, dt2) > 0) {
        printf("A data 1 é posterior a data 2!\\n");
    } else if (strcmp(dt1, dt2) < 0) {
        printf("A data 1 é anterior a data 2!\\n");
    } else {
        printf("As datas são iguais!\\n");
    }

    return 0;
}
```
Saída:
```
A data 1 é anterior a data 2!
A data 1 é anterior a data 2!
```
Note que, no primeiro exemplo, utilizamos a função `difftime` para calcular a diferença entre duas datas em segundos e comparar os resultados. Já no segundo exemplo, utilizamos a função `strcmp` para comparar duas strings no formato de data.

## Mergulho Profundo:
Comparar datas nem sempre foi tão fácil quanto é hoje. Nas versões anteriores da linguagem C, era necessário converter as datas em strings e realizar uma série de verificações manuais para determinar a relação entre elas. Além disso, existem outros métodos alternativos para comparar datas, como o uso de bibliotecas externas.

Em relação à implementação interna da função `difftime` e `strcmp`, elas utilizam algoritmos de comparação de strings e cálculo de diferença de datas específicos da linguagem C, que são otimizados para um desempenho eficiente.

## Veja também:
- [Documentação da função difftime na linguagem C](https://www.cplusplus.com/reference/ctime/difftime/)
- [Documentação da função strcmp na linguagem C](https://www.cplusplus.com/reference/cstring/strcmp/)
- [Biblioteca date.h para trabalhar com datas em C](https://github.com/HowardHinnant/date)