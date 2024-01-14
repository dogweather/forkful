---
title:    "C++: Convertendo uma data em uma sequência de caracteres."
keywords: ["C++"]
---

{{< edit_this_page >}}

## Por que converter uma data em uma string?

Ao trabalhar com programação, é comum ter que lidar com dados de diferentes tipos e formatos. Em alguns casos, pode ser necessário converter uma data, que é um dado do tipo "tempo", em uma string, que é um tipo de dado de texto. Isso pode ser útil, por exemplo, para exibir uma data em um formato específico ou para armazená-la em um banco de dados. Neste artigo, vamos explorar como podemos realizar essa conversão em C++.

## Como fazer?

A biblioteca padrão do C++ oferece algumas funções que nos ajudam a trabalhar com datas e strings. Para converter uma data em uma string, podemos usar a função `std::to_string()`, que recebe como parâmetro um objeto do tipo `std::chrono::time_point` e retorna uma string contendo a data formatada em formato ISO-8601. Veja um exemplo:

```C++
#include <iostream>
#include <chrono>
#include <string>

int main() {
    // cria um objeto time_point com a data e hora atuais
    auto data_atual = std::chrono::system_clock::now();

    // converte a data em uma string
    std::string data_string = std::to_string(data_atual);

    // imprime a data e hora no formato ISO-8601
    std::cout << "Data atual em formato ISO-8601: " << data_string << "\n";

    return 0;
}

```

**Saída:**

```
Data atual em formato ISO-8601: 2021-12-03 15:30:15
```

Outra opção é usar a função `strftime()`, que permite que formatemos a data como desejarmos. No exemplo abaixo, a data é formatada como "Dia, Mês Nome_do_Mês Ano":

```C++
#include <iostream>
#include <ctime>

int main() {
    // cria um objeto time_t com a data e hora atuais
    time_t data_atual = time(0);

    // cria um buffer para armazenar a data formatada
    char data_buffer[80];

    // formata a data usando strftime()
    strftime(data_buffer, 80, "Data atual: %A, %B %d %Y", localtime(&data_atual));

    // imprime a data formatada
    std::cout << data_buffer << "\n";

    return 0;
}

```

**Saída:**

```
Data atual: Sexta-feira, Dezembro 03 2021
```

## Aprofundando mais

Conforme mencionado anteriormente, a biblioteca padrão do C++ possui funções específicas para manipulação de datas e strings, como `std::to_string()` e `strftime()`. Além disso, também há bibliotecas externas que podem ser úteis para trabalhar com datas, como a biblioteca *Boost.Date_Time*, que fornece ferramentas para manipulação de datas e horas com maior precisão e flexibilidade.

Além disso, vale ressaltar que a conversão de uma data em uma string pode não ser uma tarefa tão simples quanto parece. Existem diferentes formas de representar uma data em uma string e cada uma delas pode ser mais adequada para determinado contexto. Portanto, é importante considerar essas variações e escolher a melhor abordagem de acordo com os requisitos do seu projeto.

## Veja também

- [Referência da biblioteca padrão do C++ para manipulação de datas e horas](https://en.cppreference.com/w/cpp/chrono)
- [Documentação da biblioteca Boost.Date_Time](https://www.boost.org/doc/libs/1_77_0/doc/html/date_time.html)
- [Como converter string para data em C++](https://www.geeksforgeeks.org/how-to-convert-a-string-to-time-in-cpp/)