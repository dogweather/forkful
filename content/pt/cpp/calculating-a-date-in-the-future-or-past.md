---
title:                "Calculando uma data no futuro ou passado"
html_title:           "C++: Calculando uma data no futuro ou passado"
simple_title:         "Calculando uma data no futuro ou passado"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## O que & Por quê?
Calcular uma data no futuro ou no passado é uma tarefa comum em programação, que envolve determinar uma data baseada em uma data inicial e um certo número de dias ou meses adicionais. Os programadores utilizam essa função para diferentes propósitos, como agendar tarefas, gerar relatórios ou desenvolver algoritmos complexos.

## Como fazer:
Para calcular uma data no futuro ou no passado em C++, podemos utilizar a biblioteca padrão <ctime> e suas funções date e time. Abaixo, dois exemplos de código mostrando como isso pode ser feito:

```
#include <iostream>
#include <ctime>

int main() {

  // Calculando 30 dias a partir de hoje
  time_t currentTime;
  time(&currentTime);

  struct tm* futureDate = localtime(&currentTime);
  futureDate->tm_mday += 30;
  mktime(futureDate);

  std::cout << "Daqui a 30 dias será " << futureDate->tm_year+1900;
  std::cout << "-" << futureDate->tm_mon+1;
  std::cout << "-" << futureDate->tm_mday << std::endl;

  // Calculando 6 meses a partir de uma data específica
  struct tm myDate = {0};
  myDate.tm_year = 120;
  myDate.tm_mon = 7;
  myDate.tm_mday = 5;

  mktime(&myDate);

  struct tm* futureMonth = localtime(&myDate);
  futureMonth->tm_mon += 6;
  mktime(futureMonth);

  std::cout << "Daqui a 6 meses será " << futureMonth->tm_year+1900;
  std::cout << "-" << futureMonth->tm_mon+1;
  std::cout << "-" << futureMonth->tm_mday << std::endl;

  return 0;
}

```
A saída para o primeiro exemplo será: "Daqui a 30 dias será [ano atual]-[mês atual]-[dia atual + 30]".

A saída para o segundo exemplo será: "Daqui a 6 meses será [ano+1]-[mês+1]-05", considerando que a data inicial foi definida como 5 de agosto de 2020.

## Mergulho profundo:
A função para calcular uma data no futuro ou no passado já existe há muito tempo e evoluiu ao longo dos anos. Antigamente, os programadores tinham que lidar com cálculos julianos para determinar datas, mas hoje em dia as linguagens de programação já oferecem funções especificas para esses cálculos, facilitando o trabalho do programador.

Além disso, também existem bibliotecas externas especializadas em manipulação de datas que oferecem diferentes funcionalidades e opções de formatação. É importante que o programador escolha a melhor opção de acordo com as necessidades do seu projeto.

Por fim, a implementação detalhada dessa função depende de como os sistemas operacionais lidam com armazenamento e manipulação de datas. Em alguns casos, pode haver diferenças, por exemplo, no tratamento de anos bissextos ou fusos horários.

## Veja também:
- [Documentação oficial da biblioteca <ctime>](https://en.cppreference.com/w/cpp/chrono/c)
- [Biblioteca Boost.Date_Time para manipulação de datas em C++](https://www.boost.org/doc/libs/1_74_0/doc/html/date_time.html)
- [Artigo sobre o cálculo de datas em diferentes linguagens de programação](https://www.bbvalabs.com/en/technology/software-development/calculate-date-future-past/)