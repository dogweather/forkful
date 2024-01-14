---
title:                "C++: Calculando uma data no futuro ou passado"
programming_language: "C++"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por que calcular uma data no futuro ou no passado?

Calcular datas no futuro ou no passado é uma habilidade útil na programação, pois permite que os desenvolvedores lidem com cenários onde precisam lidar com datas além do presente. Isso pode ser útil em uma ampla gama de aplicativos, desde agendas eletrônicas até sistemas de reservas online.

## Como fazer isso em C++

Para calcular uma data no futuro ou no passado em C++, é preciso primeiro entender o conceito de "data". Em programação, uma data é normalmente representada por uma variável do tipo "struct", contendo informações como dia, mês e ano. Para calcular uma data no futuro ou no passado a partir de uma data atual, é necessário usar a função "mktime" para converter a data em um tempo absoluto e, em seguida, adicionar ou subtrair a quantidade de segundos correspondente à diferença desejada.

Um exemplo de código pode ser visto abaixo:

```
#include <iostream>
#include <ctime>

using namespace std;

int main()
{
    // Definindo a data atual
    struct tm data_atual = {0};
    data_atual.tm_year = 2021 - 1900; // Ano atual - 1900
    data_atual.tm_mon = 3 - 1; // Mês atual - 1
    data_atual.tm_mday = 20; // Dia atual

    // Imprimindo a data atual
    cout << "Data atual: " << data_atual.tm_mday << "/" << data_atual.tm_mon + 1 << "/" << data_atual.tm_year + 1900 << endl;

    // Convertendo a data para tempo absoluto
    time_t tempo_atual = mktime(&data_atual);

    // Calculando uma data futura - 10 dias
    tempo_atual += 10 * 24 * 60 * 60; // Adicionando 10 dias em segundos

    // Convertendo o tempo absoluto de volta para uma struct de data
    struct tm data_futura = *localtime(&tempo_atual);

    // Imprimindo a data futura
    cout << "Data futura: " << data_futura.tm_mday << "/" << data_futura.tm_mon + 1 << "/" << data_futura.tm_year + 1900 << endl;

    return 0;
}

```

Ao executar esse código, o output será:

```
Data atual: 20/3/2021
Data futura: 30/3/2021
```

## Uma análise mais profunda

Além de adicionar ou subtrair uma quantidade fixa de segundos, também é possível calcular uma data no futuro ou no passado a partir de uma data específica, como uma data de nascimento ou uma data de início de um projeto. Para isso, é necessário utilizar funções mais complexas de manipulação de datas, como a função "difftime", que calcula a diferença de tempo entre duas datas, ou a função "mktime" com valores diferentes nos campos da struct de data.

Também é importante lembrar que ao fazer cálculos com datas, é necessário levar em consideração a diferença nos fusos horários e possíveis alterações no horário de verão.

## Veja também

- [Tutorial sobre datas em C++](https://www.cplusplus.com/doc/tutorial/time/)
- [Documentação oficial da função mktime](https://www.cplusplus.com/reference/ctime/mktime/)
- [Manipulando datas e horários em C++](https://www.cprogramming.com/tutorial/time.html)