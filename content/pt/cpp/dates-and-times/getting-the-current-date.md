---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:10.543236-07:00
description: "Recuperar a data atual em C++ \xE9 uma tarefa fundamental para programas\
  \ que precisam processar ou exibir datas baseadas no rel\xF3gio do sistema. \xC9\
  \ essencial\u2026"
lastmod: '2024-02-25T18:49:44.507565-07:00'
model: gpt-4-0125-preview
summary: "Recuperar a data atual em C++ \xE9 uma tarefa fundamental para programas\
  \ que precisam processar ou exibir datas baseadas no rel\xF3gio do sistema. \xC9\
  \ essencial\u2026"
title: Obtendo a data atual
---

{{< edit_this_page >}}

## O quê & Por quê?
Recuperar a data atual em C++ é uma tarefa fundamental para programas que precisam processar ou exibir datas baseadas no relógio do sistema. É essencial para logging, registro de data e hora, agendamento de tarefas e qualquer funcionalidade que dependa de datas e horas.

## Como fazer:
C++ oferece várias maneiras de obter a data atual, incluindo a biblioteca padrão do C++ e bibliotecas de terceiros como a Boost. Os exemplos a seguir demonstram como realizar essa tarefa.

### Usando `<chrono>` (C++20 e posteriores)
C++20 introduziu mais funcionalidades na biblioteca `<chrono>`, tornando-a direta para obter a data atual:
```cpp
#include <iostream>
#include <chrono>
#include <format> // Para std::format (C++20)

int main() {
    auto current_time_point = std::chrono::system_clock::now(); // Captura o tempo atual
    auto current_time_t = std::chrono::system_clock::to_time_t(current_time_point); // Converte para time_t

    // Formata o tempo para um formato legível
    std::cout << "Data Atual: " << std::format("{:%Y-%m-%d}", std::chrono::system_clock::to_time_t(current_time_point)) << std::endl;

    return 0;
}
```
**Exemplo de Saída:**
```plaintext
Data Atual: 2023-03-15
```

### Usando `<ctime>`
Para programadores que trabalham com versões anteriores do C++ ou aqueles que preferem a biblioteca tradicional C:
```cpp
#include <iostream>
#include <ctime>

int main() {
    std::time_t t = std::time(0); // Obtém o tempo atual
    std::tm* agora = std::localtime(&t);
    std::cout << "Data Atual: " 
              << (agora->tm_year + 1900) << '-' 
              << (agora->tm_mon + 1) << '-'
              <<  agora->tm_mday
              << std::endl;

    return 0;
}
```
**Exemplo de Saída:**
```plaintext
Data Atual: 2023-03-15
```

### Usando Boost Date_Time
Para projetos que utilizam as bibliotecas Boost, a biblioteca Boost Date_Time oferece um método alternativo para obter a data atual:
```cpp
#include <iostream>
#include <boost/date_time.hpp>

int main() {
    // Obtém o dia atual usando o calendário gregoriano da Boost
    boost::gregorian::date hoje = boost::gregorian::day_clock::local_day();
    std::cout << "Data Atual: " << hoje << std::endl;

    return 0;
}
```
**Exemplo de Saída:**
```plaintext
Data Atual: 2023-Mar-15
```
Esses exemplos fornecem uma base fundamental para trabalhar com datas em C++, crucial para uma ampla gama de aplicações.
