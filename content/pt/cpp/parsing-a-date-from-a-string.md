---
title:                "Analisando uma data a partir de uma string"
date:                  2024-01-20T15:35:08.224658-07:00
html_title:           "Arduino: Analisando uma data a partir de uma string"
simple_title:         "Analisando uma data a partir de uma string"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Analisar uma data a partir de uma string significa extrair informações de data e hora de um texto. Programadores fazem isso para manipular e utilizar datas de maneira eficiente em sistemas e aplicações.

## Como Fazer:

Para analisar datas de strings em C++, podemos usar a biblioteca `chrono` do C++20 junto com `stringstream` da biblioteca `<sstream>` e formatações personalizadas. Aqui está um exemplo:

```C++
#include <iostream>
#include <sstream>
#include <chrono>
#include <format>

int main() {
    std::string data_str = "20/04/2023";
    std::istringstream iss(data_str);
    std::chrono::sys_days data;
    
    iss >> std::chrono::parse("%d/%m/%Y", data);
    
    if (iss.fail()) {
        std::cout << "Análise falhou." << std::endl;
    } else {
        std::cout << "Análise bem-sucedida: " 
                  << std::format("{:%d de %B de %Y}", data) << std::endl;
    }
    
    return 0;
}
```

Output:
```
Análise bem-sucedida: 20 de abril de 2023
```

## Mergulho Profundo:

A análise de datas está presente desde o C++11 com a biblioteca `<chrono>`, mas o processo foi simplificado com o novo C++20, que introduziu uma interface de formato/parsing mais amigável. Alternativas incluem o uso da função `strptime` ou bibliotecas de terceiros como `boost::date_time`.

Detalhes de implementação envolvem entender o papel dos `locales` para configurações regionais e o manuseio de diferentes formatos de data, como ISO 8601 ou formatos locais. É crucial tratar erros como formatos inválidos ou entradas corruptas para evitar comportamento indesejado.

## Ver Também:

- [Documentação da std::chrono](https://en.cppreference.com/w/cpp/header/chrono)
- [Documentação do std::format (C++20)](https://en.cppreference.com/w/cpp/utility/format)
- [Biblioteca Boost.Date_Time](https://www.boost.org/doc/libs/release/libs/date_time/)
