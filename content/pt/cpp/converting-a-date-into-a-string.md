---
title:                "C++: Convertendo uma data em uma string"
simple_title:         "Convertendo uma data em uma string"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por que

Ao trabalhar em projetos de programação, uma tarefa comum é precisar converter uma data em um formato legível para ser exibida ao usuário. Neste artigo, aprenderemos como realizar essa conversão em C++, permitindo que nossos programas apresentem datas de forma mais amigável.

## Como Fazer

Existem várias formas de converter uma data em uma string em C++. Aqui, apresentamos dois métodos comumente utilizados: usando a biblioteca padrão "ctime" e a biblioteca "boost date_time".

### Usando a biblioteca padrão "ctime"

```C++
#include <iostream> 
#include <ctime> 
using namespace std; 

int main() 
{ 
    // Definindo uma variável tm (struct que armazena informações sobre data e hora)
    struct tm data; 

    // Definindo uma string com a data desejada
    string data_original = "2020/10/25"; 

    // Usando a função strptime para converter a string em uma struct tm
    strptime(data_original.c_str(), "%Y/%m/%d", &data); 

    // Usando a função strftime para converter a struct tm em uma string no formato desejado
    char data_string[11]; 
    strftime(data_string, 11, "%d/%m/%Y", &data); 

    // Imprimindo a string resultante
    cout << "A data convertida é: " << data_string << endl; 

    return 0; 
}
```
**Saída:** A data convertida é: 25/10/2020

Neste exemplo, utilizamos as funções "strptime" e "strftime" da biblioteca "ctime" para converter a string da data original em uma struct tm e, em seguida, em uma string no formato desejado. Um detalhe importante a se observar é o uso do formato "%Y/%m/%d" na função "strptime", que indica que a string de data original está no formato "Ano/Mês/Dia".

### Usando a biblioteca "boost date_time"

```C++
#include <iostream> 
#include <boost/date_time.hpp> 
using namespace std; 
using namespace boost::gregorian; 

int main() 
{ 
    // Definindo uma string com a data desejada 
    string data_original = "2020-Oct-25"; 
    
    // Usando a função from_string para converter a string em um objeto date 
    date data = from_string(data_original); 
    
    // Usando a função to_simple_string para converter o objeto date em uma string no formato desejado 
    cout << "A data convertida é: " << to_simple_string(data) << endl; 
    
    return 0;
}
```

**Saída:** A data convertida é: 25-Oct-2020

Neste exemplo, utilizamos as funções "from_string" e "to_simple_string" da biblioteca "boost date_time" para converter diretamente a string de data original em uma string no formato desejado. Vale ressaltar que, para utilizar a biblioteca "boost date_time", é necessário instalá-la e configurá-la corretamente em seu ambiente de desenvolvimento.

## Deep Dive

Conversão de data em string pode parecer uma tarefa simples em um primeiro momento, mas existem alguns pontos importantes a serem considerados. Alguns deles são:

- A biblioteca "ctime" é limitada em relação aos formatos de data que pode converter e, em algumas plataformas, pode apresentar problemas relacionados a idioma e localização.
- A biblioteca "boost date_time" oferece mais opções de formatos, mas sua instalação e configuração podem ser um pouco mais complexas.
- É importante que a string de data original esteja no formato correto, caso contrário, a conversão pode resultar em uma data inválida.
- É necessário escolher o formato adequado para a string resultante, de acordo com a forma com que se deseja exibir a data.

## Veja também

- [Documentação da biblioteca std::ctime em cppreference.com](https://fr.cppreference.com/w/cpp/chrono/c/strftime)
- [Documentação da biblioteca boost date_time](https://www.boost.org/doc/libs/1_74_0/doc/html/date_time.html)
- [Tutorial de instalação e configuração do boost date_time](https://www.boost.org/doc/libs/1_74_0/libs/date_time/doc/html/getting_started.html)