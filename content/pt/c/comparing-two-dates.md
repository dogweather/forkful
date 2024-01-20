---
title:                "Comparando duas datas"
html_title:           "C#: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## O que e por que?

Comparar duas datas é avaliar se uma data é anterior, posterior ou igual a outra. Programadores fazem isso para realizar funções de cronometragem, controle de prazos ou filtragem de informações baseadas em datas.

## Como fazer:

Aqui está um exemplo usando a função `difftime` da biblioteca de tempo:

```c
#include <time.h>
#include <stdio.h>

int main() {
    struct tm data1 = {0, 0, 0, 10, 7, 21, 0, 0, 0}; 
    struct tm data2 = {0, 0, 0, 15, 7, 21, 0, 0, 0}; 
    time_t tempo1 = mktime(&data1);
    time_t tempo2 = mktime(&data2);

    double diferenca = difftime(tempo2, tempo1);

    printf("Diferença em segundos: %.0lf\n", diferenca);

    return 0;
}
```

Aqui `data1` é 10 de agosto de 2021 e `data2` é 15 de agosto de 2021. O output será:

```
Diferença em segundos: 432000
```

## Mais Detalhes:

A função `difftime` foi introduzida no C89, e é muito usada até hoje para comparar duas datas. Uma alternativa é usar a função `memcmp`, mas tem implicações e não é recomendada.

Detalhes de implementação: A `difftime` retorna a diferença em segundos. Portanto, para converter em dias, precisamos dividir por (60*60*24). Note que a estrutura `tm` tem o mês começando em 0 (Janeiro) e o ano desde 1900.

## Ver Também:

1. Documentação do GNU para a função difftime: [https://www.gnu.org/software/libc/manual/html_node/Elapsed-Time.html](https://www.gnu.org/software/libc/manual/html_node/Elapsed-Time.html)
2. Tutorial prático sobre a biblioteca de timestamps em C: [https://beginnersbook.com/2017/08/c-timestamp/](https://beginnersbook.com/2017/08/c-timestamp/)