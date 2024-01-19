---
title:                "Convertendo uma data em uma string"
html_title:           "C++: Convertendo uma data em uma string"
simple_title:         "Convertendo uma data em uma string"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Como Converter uma Data em uma String em C++

## O Que & Porquê?

Converter uma data em uma string significa transformar uma representação de data (geralmente de um tipo específico de data) em uma cadeia de caracteres. Programadores fazem isso para facilitar a manipulação, exibição ou gravação de datas em um formato legível por humanos.

## Como fazer:

Aqui está um exemplo de como você pode fazer isso em C++. Estamos usando a biblioteca `std::put_time` para converter a data:

```C++
#include <iostream>
#include <iomanip>
#include <ctime>

int main() {
    std::time_t t = std::time(nullptr);
    std::tm* tm = std::localtime(&t);

    std::cout << std::put_time(tm, "%d-%m-%Y %H:%M:%S") << '\n';

    return 0;
}
```

Este programa irá imprimir a data e a hora atuais no seguinte formato: *DD-MM-AAAA HH:MM:SS*.

## Mergulho Profundo

Converter uma data em uma string pode parecer um procedimento simples, mas tem um contexto histórico bem profundo. Antes da padronização, cada fabricante ou equipe de software tinha sua própria maneira de representar datas. Isso levou ao fenômeno conhecido como o problema do ano 2000 (ou Y2K).

Existem também outras bibliotecas para conversão de data em string em C++. Algumas delas, como Boost Date_Time, possuem uma variedade de funções que oferecem mais flexibilidade.

Em relação ao `std::put_time`, por trás dos panos, a função pega uma estrutura tm e um determinado formato, e converte isso em uma string de acordo com o formato fornecido. Isso facilita que os programadores representem datas no formato desejado.

## Veja Também

1. Para obter mais informações sobre a biblioteca `std::put_time` do C++, consulte [aqui](http://www.cplusplus.com/reference/iomanip/put_time/).
2. Para um guia mais aprofundado de datas e horários no C++, veja [este link](http://www.cplusplus.com/doc/tutorial/ctime/).
3. Para saber mais sobre a biblioteca Boost Date_Time, veja [este link](https://www.boost.org/doc/libs/1_76_0/doc/html/date_time.html).