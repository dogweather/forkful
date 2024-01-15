---
title:                "Calculando uma data no futuro ou no passado"
html_title:           "C++: Calculando uma data no futuro ou no passado"
simple_title:         "Calculando uma data no futuro ou no passado"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por que

Às vezes, precisamos calcular uma data no futuro ou no passado em nossos programas. Isso pode ser útil para agendamento, previsões ou até mesmo para exibir informações sobre eventos específicos.

## Como Fazer

Para calcular uma data no futuro ou no passado em C++, podemos usar a biblioteca de data e hora padrão, chamada `chrono`. Usando essa biblioteca, podemos definir uma data específica como ponto de partida e adicionar ou subtrair um determinado número de dias para obter a data desejada.

Primeiro, vamos incluir a biblioteca `chrono` em nosso código:

```C++
#include <chrono>
```

Agora, vamos definir a data inicial usando a classe `std::chrono::system_clock`. Esta classe representa o relógio do sistema e fornece informações sobre a hora atual do sistema.

```C++
std::chrono::system_clock::time_point data_inicial = std::chrono::system_clock::now();
```

Em seguida, podemos adicionar ou subtrair um determinado número de dias usando a função `std::chrono::duration` e definindo o período em `days`.

```C++
std::chrono::duration<int, std::ratio<86400>> dias(5); //adiciona 5 dias à data atual
```

Agora, podemos usar a função `std::chrono::operator+` para adicionar esse período à nossa data inicial e obter a data no futuro.

```C++
std::chrono::system_clock::time_point data_final = data_inicial + dias; // data final com 5 dias a mais
```

Para exibir a data em um formato legível, podemos convertê-la para um tipo de dados `std::tm` usando a função `std::chrono::system_clock::..
to_time_t` e, em seguida, usar a função `std::localtime` para formatá-la.

```C++
std::tm data_formatada = *std::localtime(&std::chrono::system_clock::to_time_t(data_final));
```

Agora, podemos exibir a data no formato desejado usando a função `std::strftime`.

```C++
std::cout << "Data futura: " << std::put_time(&data_formatada, "%d/%m/%Y") << '\n';
```

Para calcular uma data no passado, basta subtrair o período desejado em vez de adicioná-lo.

## Mergulho Profundo

A função `std::chrono::system_clock::time_point` representa um ponto específico no tempo, enquanto a função `std::chrono::duration` representa um período de tempo. Esses tipos de dados podem ser personalizados para se adequar às necessidades do programador. Além disso, a biblioteca `chrono` também inclui funções para lidar com diferentes fusos horários e formatos de data.

## Veja Também

- [Documentação da biblioteca chrono do C++](https://en.cppreference.com/w/cpp/chrono)
- [Tutorial sobre a biblioteca chrono do C++](https://www.geeksforgeeks.org/chrono-in-c/)
- [Exemplo de cálculo de data no futuro em C++](https://www.techbeamers.com/calculate-date-and-time-in-cpp/)