---
title:                "Obtendo a data atual"
html_title:           "C: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Obtendo A Data Atual em C++

## O Que & Por quê?

Obter a data atual é um processo que retorna a data e a hora atuais do sistema. Programadores costumam fazer isso quando precisam registrar eventos, criar um registro de atividades ou controlar o tempo.

## Como Fazer:
Aqui estão exemplos de como usar a biblioteca Chrono, disponível no C++ (versão atual):

```C++
#include <iostream>
#include <chrono>
#include <ctime>

int main() {
    auto agora = std::chrono::system_clock::now();
    std::time_t tempo_agora = std::chrono::system_clock::to_time_t(agora);

    std::cout << "Data e hora atuais: " << std::ctime(&tempo_agora) << std::endl;

    return 0;
}
```

Ao rodar este código, você deverá ver algo similar ao seguinte na sua tela:

```C++
Data e hora atuais: Tue Sep 14 20:21:39 2021
```

## Mergulhando Mais Fundo

Historicamente, C++ não possuía suas próprias funções de data e hora. Costumava-se usar as funções da biblioteca C `ctime`. No C++11, a biblioteca `chrono` foi introduzida, oferecendo um manejo de tempo mais preciso e flexível.

Existem muitas alternativas para obter a data atual em C++. Uma delas é usando a função `time()` da biblioteca C. No entanto, é recomendado usar `chrono` pois ela é mais precisa e tem uma API mais consistente.

A função `system_clock::now()` da biblioteca `chrono` retorna o tempo atual como um objeto `time_point`. Para converter isso em uma string legível, primeiro precisamos convertê-lo para `time_t` usando `to_time_t()`, e depois podemos usar `ctime()` para converter esse `time_t` em string.

## Veja também

Aqui estão alguns links para fontes relacionadas que podem ser úteis:
- Documentação da biblioteca Chrono: [https://en.cppreference.com/w/cpp/chrono](https://en.cppreference.com/w/cpp/chrono)
- Tutorial de date e time em C++: [http://www.cplusplus.com/reference/ctime/](http://www.cplusplus.com/reference/ctime/)
- Guia de Programação em C++ da Microsoft: [https://docs.microsoft.com/en-us/cpp/cpp/?view=msvc-160](https://docs.microsoft.com/en-us/cpp/cpp/?view=msvc-160)