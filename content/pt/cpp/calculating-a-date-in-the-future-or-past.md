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

---

## O Que & Por Quê?

Calcular uma data futura ou passada significa definir uma data que está um certo número de dias antes ou depois de uma data específica. Os programadores o fazem para rastrear períodos de tempo, gerar relatórios cronológicos ou desenvolver funções de lembrete.

## Como Fazer:

A biblioteca `chrono` do C++ oferece funcionalidades robustas. Aqui está um exemplo simples para calcular uma data futura :

```C++
#include <iostream>
#include <chrono>
#include <ctime>

using namespace std;

int main() {
    auto now = chrono::system_clock::now();
    chrono::duration<int, ratio<60*60*24>> oneDay (1);
    auto tomorrow = now + oneDay;

    time_t tt = chrono::system_clock::to_time_t(tomorrow);

    cout << "Amanhã é " << ctime(&tt);

    return 0;
}
```

Neste script, adicionamos 1 dia à data atual. Exemplo de saída:

```
Amanhã é Sun Mar 14 14:52:20 2022
```

## Deep Dive

No passado, os programadores geralmente lidavam com operações de data e hora diretamente, o que era propenso a erros. A introdução do `chrono` no C++11 mudou o jogo, fornecendo métodos precisos para calcular datas futuras ou passadas.

Existem alternativas, como a função `mktime()` da biblioteca `ctime`, mas `chrono` oferece uma abordagem mais segura e intuitiva ao lidar com as datas.

Com `chrono`, você pode manipular datas usando `time_point` (um ponto no tempo) e expressar durações de várias maneiras (horas, minutos, segundos, dias). Além disso, a biblioteca `chrono` considera problemas como meses de diferentes durações e anos bissextos, simplificando o cálculo de datas futuras ou passadas.

## Veja Também

1. Documentação do Chrono: http://www.cplusplus.com/reference/chrono/
2. Artigo: "Data e Hora em C++" - https://www.learncpp.com/cpp-tutorial/date-and-time/
3. Documentação do Ctime: http://www.cplusplus.com/reference/ctime/
4. Bibliotecas alternativas DateTime em cpp: https://www.boost.org/doc/libs/1_63_0/doc/html/date_time.html