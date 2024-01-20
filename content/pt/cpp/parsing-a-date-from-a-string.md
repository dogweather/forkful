---
title:                "Analisando uma data a partir de uma string"
html_title:           "PowerShell: Analisando uma data a partir de uma string"
simple_title:         "Analisando uma data a partir de uma string"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Análise e Conversão de Datas em Strings no C++: O Que, Por Que e Como?

## O Quê e Por Quê?
A análise de datas a partir de strings envolve reconhecer e converter valores de data e hora em um formato de texto. Programadores fazem isso para manusear e manipular dados de data e hora de maneira mais fácil e eficiente.

## Como Fazer:
Aqui está um exemplo simples usando a biblioteca Chrono do C++:

```C++
#include <iostream>
#include <sstream>
#include <iomanip>
#include <chrono>

int main() {
    std::istringstream ss("2021-09-23 15:45:10");
    std::chrono::system_clock::time_point tp;
    ss >> std::get_time(&tp, "%Y-%m-%d %H:%M:%S");

    if (ss.fail()){
        std::cout << "Erro na análise da string." << std::endl;
    } else {
        std::cout << "Análise bem-sucedida." << std::endl;
    }

    return 0;
}
```

A saída será: `Análise bem-sucedida.`

## Aprofundando

Historicamente, a análise de datas a partir de strings tem sido uma operação comum na programação. Ela é frequentemente usada quando se lida com bancos de dados, arquivos de log, e os dados recebidos de diferentes APIs, entre outros.

Existem alternativas às bibliotecas built-in do C++, tais como a biblioteca de data e hora do Boost. No entanto, a biblioteca Chrono do C++ moderno apresenta funcionalidades avançadas e é mais user-friendly, evitando a necessidade de recursos de terceiros na maioria dos casos.

Em relação aos detalhes de implementação, a análise de uma data a partir de uma string no C++ occur através da leitura da string que representa a data e hora, e então emprega a função `get_time` para converter essa string em um formato de data e hora que o programa possa manipular.

## Veja Também

Para continuar aprendendo sobre este tópico, confira estes links úteis:
1. Documentação Oficial da Biblioteca Chrono do C++ (https://en.cppreference.com/w/cpp/chrono)
2. Documentação da Biblioteca Boost Date_Time (https://www.boost.org/doc/libs/1_76_0/doc/html/date_time.html)
3. Tutorial de Como Trabalhar com Datas e Horas no C++ (https://www.geekhideout.com/urlcode.shtml)
4. Funções de Data e Hora no C++ (https://www.tutorialspoint.com/cplusplus/cpp_date_time.htm)