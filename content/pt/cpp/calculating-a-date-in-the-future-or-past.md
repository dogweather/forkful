---
title:                "Calculando uma data no futuro ou passado"
date:                  2024-01-20T17:30:54.641149-07:00
model:                 gpt-4-1106-preview
simple_title:         "Calculando uma data no futuro ou passado"

category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Calcular uma data no futuro ou passado significa manipular datas para encontrar dias específicos antes ou depois de um ponto no tempo. Programadores fazem isso para agendar eventos, calcular prazos ou simplesmente registrar quando algo aconteceu ou acontecerá.

## Como Fazer:
```C++
#include <iostream>
#include <ctime>

int main() {
    // Pega o tempo atual
    std::time_t atual = std::time(nullptr);

    // Converte para struct tm para manipulacao mais facil
    struct tm data = *std::localtime(&atual);

    // Altera a data: 30 dias no futuro
    data.tm_mday += 30;
    // Normaliza a struct tm e converte de volta para time_t
    std::time_t futuro = std::mktime(&data);

    // Mostra a nova data
    std::cout << "Data Futura: " << std::asctime(&data) << std::endl;

    return 0;
}
```
Exemplo de saída:
```
Data Futura: Wed Feb 26 14:55:21 2023
```

## Mergulho Profundo
Calcular datas não é algo novo – tem sido fundamental desde que começamos a medir o tempo. Em C++, nós já vimos a `<ctime>` e a `<chrono>` para lidar com o tempo. A `<ctime>` te dá uma maneira mais tradicional e baseada em C de tratar tempo, enquanto `<chrono>`, introduzida no C++11, te oferece uma abordagem moderna e tipicamente mais segura.

Antes de `<chrono>`, manipular datas exigia atenção cuidadosa, principalmente devido a possíveis erros ao normalizar datas (ajustando os dias e meses para valores válidos após manipulações).

Além disso, várias bibliotecas de terceiros, como a Boost.Date_Time, oferecem funcionalidades avançadas se você precisar de algo mais complexo.

Detalhes de implementação normalmente envolvem lidar com o formato de data e hora (struct tm), normalização de tempo (usando `mktime()` para ajustar struct tm) ou calcular diferenças de tempo.

## Veja Também

- Documentação da biblioteca `<ctime>`: https://en.cppreference.com/w/cpp/header/ctime
- Documentação da biblioteca `<chrono>`: https://en.cppreference.com/w/cpp/header/chrono
- Biblioteca Boost.Date_Time: https://www.boost.org/doc/libs/release/libs/date_time/
