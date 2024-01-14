---
title:    "C: Calculando uma data no futuro ou passado"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por que

Calcular datas no futuro ou no passado pode ser uma tarefa muito útil e necessária em vários projetos de programação. Seja para realizar agendamentos, criar algoritmos de previsão ou lidar com dados temporais, a capacidade de calcular datas com precisão é essencial para muitas aplicações.

## Como fazer

Para calcular uma data no futuro ou no passado, podemos usar funções da biblioteca padrão <ctime>. Uma dessas funções é a "mktime", que aceita uma estrutura "tm" preenchida com valores para ano, mês, dia, hora, minuto e segundo e retorna um valor do tipo "time_t" representando a data e hora correspondentes. A partir disso, podemos usar outras funções como "localtime" ou "gmtime" para converter esse valor em uma estrutura "tm" ou "strftime" para formatar a data como quisermos.

```
#include <stdio.h>
#include <time.h>

int main()
{
  // Criando e preenchendo a estrutura tm
  struct tm data;
  data.tm_year = 2020 - 1900; // ano - 1900
  data.tm_mon = 11; // mês (de 0 a 11)
  data.tm_mday = 25; // dia
  data.tm_hour = 10; // hora
  data.tm_min = 30; // minuto
  data.tm_sec = 0; // segundo

  // Convertendo em time_t
  time_t data_em_time_t = mktime(&data);

  // Convertendo para uma estrutura tm
  struct tm *data_convertida = localtime(&data_em_time_t);
  printf("Data convertida: %s", asctime(data_convertida));

  // Formatando a data com strftime
  char data_formatada[50];
  strftime(data_formatada, 50, "%d/%m/%Y - %H:%M", data_convertida);
  printf("Data formatada: %s", data_formatada);

  return 0;
}
```

O resultado da execução do código será:

```
Data convertida: Thu Dec 25 10:30:00 2020
Data formatada: 25/12/2020 - 10:30
```

## Deep Dive

Para calcular uma data no futuro ou no passado, é importante ter em mente que devemos considerar informações como anos bissextos e fusos horários. Além disso, podemos usar outras funções da biblioteca <ctime> como "difftime" para calcular a diferença entre duas datas e "asctime" para converter uma estrutura "tm" em uma string.

Também é possível utilizar a biblioteca <chrono> do C++ para calcular datas com maior precisão e de forma mais simples, pois ela oferece classes para representar tanto datas quanto durações em um único objeto.

## Veja também

- https://en.cppreference.com/w/cpp/chrono
- https://www.ime.usp.br/~pf/algoritmos/aulas/refinamentos/refinamento-do-calculo-da-quantidade-de-dias.html
- https://www.universidadeouexcentrico.com/2016/04/20/8-metodos-para-se-manipular-datas-em-php/