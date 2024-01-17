---
title:                "Comparando duas datas"
html_title:           "C++: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## O que e porque?
Comparar duas datas significa verificar se uma data é anterior, posterior ou igual a outra data. Esse processo é útil para programadores que precisam ordenar ou filtrar dados com base em datas.

## Como fazer:
```C++
#include <iostream>
#include <ctime>

int main() {
  // Definindo as datas a serem comparadas
  struct tm t1 = {0, 0, 0, 1, 0, 119, 0, 0, -1}; // 01/01/2019
  struct tm t2 = {0, 0, 0, 1, 0, 120, 0, 0, -1}; // 01/01/2020

  // Convertendo as datas para o formato de timestamp
  time_t timestamp1 = mktime(&t1);
  time_t timestamp2 = mktime(&t2);

  // Comparando as datas
  if (timestamp1 < timestamp2) {
    std::cout << "A data 01/01/2019 é anterior a 01/01/2020." << std::endl;
  } else if (timestamp1 > timestamp2) {
    std::cout << "A data 01/01/2019 é posterior a 01/01/2020." << std::endl;
  } else {
    std::cout << "As datas 01/01/2019 e 01/01/2020 são iguais." << std::endl;
  }

  return 0;
}
```
Output:
```
A data 01/01/2019 é anterior a 01/01/2020.
```

## Mergulho profundo:
Comparar datas é uma tarefa importante em programação, pois muitas vezes é necessário trabalhar com dados em ordem cronológica. Existem outras formas de realizar essa comparação, como utilizar funções disponíveis em bibliotecas de terceiros. Internamente, a comparação de datas em C++ é feita convertendo as datas para o formato de timestamp e comparando os valores inteiros.

## Veja também:
- [Documentação da função `mktime` em C++](https://www.cplusplus.com/reference/ctime/mktime/)
- [Biblioteca Boost.Date_Time para manipulação de datas em C++](https://www.boost.org/doc/libs/1_71_0/doc/html/date_time.html)