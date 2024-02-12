---
title:                "Comparando duas datas"
date:                  2024-02-03T17:53:41.619340-07:00
model:                 gpt-4-0125-preview
simple_title:         "Comparando duas datas"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/comparing-two-dates.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Que?

Comparar duas datas em C envolve determinar a relação cronológica entre elas -- se uma data precede a outra ou se são iguais. Essa capacidade é crucial em aplicações que lidam com agendamentos, prazos ou registro de atividades, pois permite a organização e manipulação de dados sensíveis ao tempo.

## Como Fazer:

C não possui um tipo embutido para datas, o que torna necessário o uso da biblioteca `time.h` para trabalhar com estruturas de data e tempo. A estrutura `tm` e a função `difftime()` são comumente usadas para comparar datas. Abaixo está um exemplo de como comparar duas datas:

```c
#include <stdio.h>
#include <time.h>

int main() {
    struct tm date1 = {0};
    struct tm date2 = {0};
    double seconds;

    // Primeira data (AAAA, MM, DD)
    date1.tm_year = 2023 - 1900; // Ano desde 1900
    date1.tm_mon = 3 - 1;        // Mês [0-11]
    date1.tm_mday = 15;          // Dia do mês [1-31]

    // Segunda data (AAAA, MM, DD)
    date2.tm_year = 2023 - 1900;
    date2.tm_mon = 4 - 1;
    date2.tm_mday = 14;

    // Converter para formato time_t
    time_t time1 = mktime(&date1);
    time_t time2 = mktime(&date2);

    // Comparar
    seconds = difftime(time1, time2);

    if (seconds == 0) {
        printf("As datas são iguais.\n");
    } else if (seconds > 0) {
        printf("A primeira data vem depois da segunda data.\n");
    } else {
        printf("A primeira data vem antes da segunda data.\n");
    }

    return 0;
}
```

A saída pode ser:

```text
A primeira data vem antes da segunda data.
```

Esse programa inicializa duas estruturas `tm` com datas específicas, converte-as para o formato `time_t` usando `mktime()`, e finalmente as compara usando `difftime()`, que retorna a diferença em segundos (como um `double`) entre os dois tempos.

## Aprofundamento

Nos primeiros dias de C, as operações de data e hora requeriam cálculos manuais, muitas vezes levando em conta anos bissextos, o número variável de dias nos meses e até segundos intercalares. A introdução de `time.h` no padrão ANSI C trouxe padronização para o manuseio do tempo em C, simplificando as operações de data e hora.

Usar `time.h` para comparação de datas é simples, mas tem limitações. A estrutura `tm` não leva em conta fusos horários ou horário de verão, e `difftime()` só fornece a diferença em segundos, faltando granularidade mais fina para certas aplicações.

Para aplicações que requerem operações de data-hora mais robustas, incluindo suporte para fusos horários, transições de horário de verão e intervalos de tempo mais precisos, bibliotecas como `date.h` (uma biblioteca de data de Howard Hinnant, não parte da biblioteca padrão) oferecem uma alternativa moderna a `time.h`. Essas bibliotecas fornecem ferramentas mais abrangentes para manipulação de data-hora em C++, beneficiando-se de décadas de evolução no design de linguagens de programação. Para programadores em C, o uso dessas bibliotecas externas ou o manuseio meticuloso das peculiaridades dos cálculos de data-hora diretamente permanece necessário para alcançar manipulação de data-hora precisa e culturalmente consciente.
