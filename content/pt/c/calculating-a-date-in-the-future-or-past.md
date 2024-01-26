---
title:                "Calculando uma data no futuro ou passado"
date:                  2024-01-20T17:28:32.782766-07:00
model:                 gpt-4-1106-preview
html_title:           "Clojure: Calculando uma data no futuro ou passado"
simple_title:         "Calculando uma data no futuro ou passado"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Calcular uma data no futuro ou passado é simplesmente ajustar uma data conhecida por um certo número de dias, meses ou anos. Programadores fazem isso para criar lembretes, agendar eventos, validar prazos ou gerir assinaturas.

## Como Fazer:

A função `mktime` e a estrutura `struct tm` da biblioteca padrão de C são suas aliadas. Aqui está como você usaria elas:

```c
#include <stdio.h>
#include <time.h>

int main() {
    struct tm data = {0};
    double dias = 90; // Número de dias para ajustar
    
    // Configurando a data inicial (1º de Janeiro de 2023)
    data.tm_year = 2023 - 1900; // Anos desde 1900
    data.tm_mon = 0;          // Mês de Janeiro
    data.tm_mday = 1;         // Primeiro dia do mês

    // Converte struct tm para time_t
    time_t data_inicial = mktime(&data);

    // Adiciona ou subtrai dias
    data_inicial += (dias * 86400); // 86400 segundos em um dia

    // Converte de volta para struct tm para ajustar qualquer desbordamento
    struct tm *data_resultante = localtime(&data_inicial);

    // Exibe a nova data
    printf("Nova data: %02d/%02d/%04d\n", 
           data_resultante->tm_mday, 
           data_resultante->tm_mon + 1,  // Meses são indexados desde 0
           data_resultante->tm_year + 1900); 

    return 0;
}
```

Saída de exemplo:

```
Nova data: 01/04/2023
```

## Aprofundamento

Historicamente, cálculo de datas não era necessário até que comunicação e transporte avançados exigissem precisão temporal. Linguagens antigas de programação não dispunham de bibliotecas de data e hora. Hoje, além da `mktime` e `time_t`, temos bibliotecas como a `date.h` em C++ ou módulos em linguagens de alto nível facilitando assim o cálculo de datas.

Implementar um cálculo de datas corretamente requer conhecimento de fuso horário, calendários, horário de verão e ano bissexto. Alternativas como a biblioteca `time.h` em C cuidam disso automaticamente, mas você pode explorar bibliotecas de terceiros com mais funcionalidades.

## Veja Também

- [A documentação da biblioteca `time.h`](https://en.cppreference.com/w/c/chrono)
- [Tutorial sobre manipulação de data e hora em C](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [Zona de horário em C e a função `localtime`](https://man7.org/linux/man-pages/man3/localtime.3.html)
- [Biblioteca `Date`](https://github.com/HowardHinnant/date), aumento da funcionalidade de `chrono` em C++
