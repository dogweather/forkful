---
title:                "Obtendo a data atual"
date:                  2024-01-20T15:13:30.913779-07:00
simple_title:         "Obtendo a data atual"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

A obtenção da data atual em C++ é o processo de recuperar a data do sistema onde o programa está sendo executado. Programadores costumam fazer isso para marcar eventos, manipular valores baseados em datas ou simplesmente registrar quando algo aconteceu.

## Como Fazer:

Aqui está um exemplo simples usando a biblioteca `<chrono>` para pegar a data atual:

```C++
#include <iostream>
#include <chrono>
#include <ctime>

int main() {
    auto now = std::chrono::system_clock::now(); // Pega o tempo atual
    std::time_t now_c = std::chrono::system_clock::to_time_t(now); // Converte para time_t

    std::cout << "Data e hora atuais: " << std::ctime(&now_c); // Exibe a data e hora
    
    return 0;
}
```

Exemplo de saída:

```
Data e hora atuais: Wed Feb 23 14:55:03 2023
```

## Aprofundamento:

Historicamente, o C++ usou a biblioteca `<ctime>` para lidar com datas e horas, que é herdada da linguagem C. No entanto, com a introdução do C++11, a biblioteca `<chrono>` trouxe uma forma mais robusta e segura de manipular tempo. 

As alternativas para obter a data podem incluir o uso de bibliotecas externas como Boost.DateTime ou a antiga `<ctime>`. A escolha entre eles pode depender de questões como portabilidade, precisão e facilidade de uso.

Quanto aos detalhes de implementação, `<chrono>` trabalha com durações, clock e períodos de forma mais precisa e menos suscetível a erros. O tipo `system_clock` representa o relógio do sistema e é tipicamente o relógio de parede em tempo real.

## Veja Também:

- Documentação oficial do C++ `<chrono>`: https://en.cppreference.com/w/cpp/chrono
- Documentação oficial do C++ `<ctime>`: https://en.cppreference.com/w/cpp/header/ctime
- Boost.DateTime: https://www.boost.org/doc/libs/release/libs/date_time/
