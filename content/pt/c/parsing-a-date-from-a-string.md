---
title:                "Analisando uma data a partir de uma string"
date:                  2024-01-20T15:35:01.365611-07:00
html_title:           "Arduino: Analisando uma data a partir de uma string"
simple_title:         "Analisando uma data a partir de uma string"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Converter uma data de string para um formato utilizável permite que os programas manipulem datas e horários. Os programadores fazem isso para validar dados, realizar comparações de tempo ou simplesmente formatar para exibição.

## Como fazer:
O exemplo abaixo demonstra como converter uma string que contém uma data para uma estrutura `tm` usando a função `strptime` da biblioteca `time.h`.
```c
#include <stdio.h>
#include <time.h>

int main() {
    const char *dataString = "2023-03-15 14:58:00";
    struct tm dataEstrutura;
    
    if (strptime(dataString, "%Y-%m-%d %H:%M:%S", &dataEstrutura) == NULL) {
        printf("Falha ao converter a data.\n");
    } else {
        printf("Data convertida com sucesso: %d-%d-%d %d:%d:%d\n",
            dataEstrutura.tm_year + 1900, // Ano desde 1900
            dataEstrutura.tm_mon + 1,     // Mês começa em 0
            dataEstrutura.tm_mday,
            dataEstrutura.tm_hour,
            dataEstrutura.tm_min,
            dataEstrutura.tm_sec);
    }
    return 0;
}
```
Saída de exemplo:
```
Data convertida com sucesso: 2023-3-15 14:58:0
```

## Aprofundando
O parsing de strings de datas tem sido uma necessidade desde que os computadores começaram a interagir com tempo e calendários. Tradicionalmente, a biblioteca padrão C inclui funções como `strptime` e `strftime` para manipular representações de tempo.

Alternativas para `strptime` em outros contextos incluem funções específicas de bibliotecas, como `getdate` em algumas implementações POSIX ou até bibliotecas de terceiros que lidam com o tempo de formas mais sofisticadas.

Em relação aos detalhes da implementação, `strptime` funciona interpretando a string de acordo com os formatos especificados. Por exemplo, `%Y` representa o ano com o século como um número decimal, `%m` o mês e `%d` o dia do mês. A estrutura `tm` é preenchida com os valores correspondentes.

## Veja Também
Para mais sobre manipulação de datas e horas em C:
- Manual do `strptime`: https://www.man7.org/linux/man-pages/man3/strptime.3.html
- Tutorial sobre a biblioteca `time.h`: https://www.tutorialspoint.com/c_standard_library/time_h.htm
- Referência da estrutura `tm`: https://en.cppreference.com/w/c/chrono/tm
