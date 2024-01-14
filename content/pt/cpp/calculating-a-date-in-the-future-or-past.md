---
title:                "C++: Calculando uma data no futuro ou no passado"
simple_title:         "Calculando uma data no futuro ou no passado"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por que
Você já precisou saber qual será a data daqui a um determinado número de dias? Ou qual data era há 10 anos atrás? A programação pode nos ajudar a responder essas perguntas de forma rápida e precisa.

## Como fazer
Para calcular uma data no futuro ou no passado em C++, precisamos utilizar a biblioteca "chrono" e suas funções e classes de gerenciamento de tempo. Primeiro, devemos incluir a biblioteca no início do nosso código:

```C++
#include <chrono>
```

Em seguida, podemos criar uma variável do tipo "chrono::system_clock::time_point" para representar a data atual:

```C++
std::chrono::system_clock::time_point data_atual = std::chrono::system_clock::now();
```

Para calcular uma data futura, podemos usar a função "std::chrono::operator+()" para adicionar uma determinada quantidade de dias à data atual:

```C++
std::chrono::system_clock::time_point data_futura = data_atual + 7 * std::chrono::hours(24); // adiciona 7 dias
```

E para calcular uma data no passado, basta usar a função "std::chrono::operator-()" para subtrair dias:

```C++
std::chrono::system_clock::time_point data_passada = data_atual - 10 * std::chrono::duration<int, std::ratio<86400>>; // subtrai 10 dias
```

Agora, podemos imprimir as datas calculadas utilizando a função "std::chrono::duration_cast<>()" para converter a "time_point" para um tipo de dados mais legível, como "std::chrono::system_clock::duration":

```C++
std::cout << "Data futura: " << std::chrono::duration_cast<std::chrono::system_clock::duration>(data_futura.time_since_epoch()).count() << std::endl;
std::cout << "Data passada: " << std::chrono::duration_cast<std::chrono::system_clock::duration>(data_passada.time_since_epoch()).count() << std::endl;
```

O resultado será a quantidade de segundos desde a "epoch time" (1 de janeiro de 1970), mas podemos formatá-la de acordo com nossas necessidades.

## Deep Dive
A razão pela qual podemos calcular datas no futuro ou no passado em C++ está nos métodos e operadores sobrecarregados da biblioteca "chrono". A função "now()" retorna a data e hora atuais em "time_point", que é uma estrutura de dados de ponto no tempo. Podemos usar os operadores "+" e "-" para adicionar ou subtrair uma duração de tempo, como horas, dias ou até mesmo segundos.

Além disso, a função "duration_cast<>()" é essencial para obtermos um tipo de dados mais legível do "time_point". Como a "time_point" é representada em segundos desde a "epoch time", podemos converter essa quantidade em qualquer unidade de tempo utilizando essa função.

Por fim, a biblioteca "chrono" também possui outras funcionalidades interessantes, como o cálculo de duração entre duas datas, ou até mesmo a exibição da data e hora em diferentes fusos horários.

## Veja também
- [Documentação oficial da biblioteca "chrono" em cplusplus.com](https://www.cplusplus.com/reference/chrono/)
- [Tutorial sobre gerenciamento de tempo em C++ no site GeeksforGeeks](https://www.geeksforgeeks.org/time-management-c-programming/)
- [Exemplo prático de cálculo de datas no futuro ou passado em C++ no GitHub](https://github.com/mohitmulchandani/Time-Management-in-C-)