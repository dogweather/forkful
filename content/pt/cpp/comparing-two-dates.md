---
title:                "Comparando duas datas"
html_title:           "C#: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## O quê & Por quê?

Comparar duas datas significa determinar qual é anterior ou posterior, ou se ambas são iguais. Como programador, essa operação ajuda a tomar decisões baseadas no tempo e a gerenciar dados datados.

## Como fazer:

Aqui está um exemplo básico de como comparar duas datas em C++.

```C++
#include <iostream>
#include <ctime>

int main() {
  // Construir duas estruturas tm
  std::tm a = {0,0,0,5,11,120}; // 5 de Dezembro de 2020
  std::tm b = {0,0,0,15,11,120}; // 15 de Dezembro de 2020
  
  // Converter para time_t
  std::time_t x = std::mktime(&a);
  std::time_t y = std::mktime(&b);
  
  if ( x != (std::time_t)(-1) && y != (std::time_t)(-1) ) {
    if (x < y) std::cout << "a é anterior a b";
    else if (y < x) std::cout << "b é anterior a a";
    else std::cout << "a e b são iguais";
  }
  
  return 0;
}
```

## Mergulho Profundo:

Historicamente, o C++ herdou suas ferramentas de manipulação de datas e horas da biblioteca da linguagem C, que já era bastante rudimentar nesse aspecto. Felizmente, a partir do C++11, as bibliotecas `<chrono>` e `<date>` foram adicionadas, oferecendo alternativas muito mais robustas e sofisticadas para trabalhar com datas.

Vale a pena notar que a função `mktime` converte um `std::tm` em `std::time_t`, representando o número de segundos desde a "época Unix", que é 1 de Janeiro de 1970. Este valor é independente do fuso horário. A comparação das datas é, então, realmente simplesmente a comparação desses contadores de segundos.

## Veja Também:

- **Biblioteca `<chrono>`**: Para operações mais complexas com datas e horas, a biblioteca `<chrono>` oferece uma gama de funções úteis. Veja mais em <https://pt.cppreference.com/w/cpp/chrono>.

- **Biblioteca `<date>`**: A biblioteca `<date>` introduzida no C++20 fornece suporte para calendário civil e fusos horários. Confira <https://en.cppreference.com/w/cpp/chrono>.