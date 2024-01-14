---
title:                "C++: Convertendo uma data em uma string."
programming_language: "C++"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por que

Converter uma data em uma string pode ser uma tarefa útil em muitos programas em C++. Por exemplo, se você estiver criando um programa de gerenciamento financeiro, pode ser necessário exibir as datas das transações em formato de string para facilitar a leitura e interpretação dos usuários.

## Como fazer

Para converter uma data em uma string em C++, você precisará usar a biblioteca padrão do C++ e a função `to_string()`. Veja um exemplo de código abaixo:

```C++
#include <iostream>
#include <string>
#include <ctime>

int main(){
  // Obtém a data atual
  time_t now = time(0);
  tm * data_hora = localtime(&now);

  // Converte a data para uma string 
  std::string data = std::to_string(data_hora->tm_mday) + "/" + std::to_string(data_hora->tm_mon + 1) + "/" + std::to_string(data_hora->tm_year + 1900);

  // Imprime a string com a data formatada
  std::cout << "A data atual é: " << data << std::endl;

  return 0;
}
```

Ao executar esse código, a saída será algo como:

```
A data atual é: 01/03/2021
```

Nesse exemplo, primeiro obtemos a data atual usando a função `time()`, e em seguida, usamos a função `to_string()` para converter as partes da data (dia, mês e ano) em strings, adicionando as barras "/" entre elas. Por fim, imprimimos a string formatada com a data atual.

## Aprofundando

A biblioteca padrão do C++ inclui várias funções úteis para trabalhar com datas e horários, como `localtime()` para obter a data e hora local, e `strftime()` para formatar uma data em uma string de acordo com um padrão específico. Você também pode usar a biblioteca Boost para lidar com datas e horários de forma mais avançada.

Além disso, é importante ter em mente a formatação da data em diferentes sistemas operacionais, pois pode variar. Portanto, é sempre recomendável testar sua implementação em diferentes plataformas para garantir que o programa funcione corretamente.

## Veja também

- [Documentação oficial do C++ sobre estruturas de datas e tempo](https://en.cppreference.com/w/cpp/chrono)
- [Guia do Boost para manipulação de datas e tempo em C++](https://www.boost.org/doc/libs/1_67_0/doc/html/date_time.html)