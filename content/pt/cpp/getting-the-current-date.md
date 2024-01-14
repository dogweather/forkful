---
title:                "C++: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por que obter a data atual em programação?

Obter a data atual em um programa pode ser útil em várias situações, como em sistemas de gestão de tarefas, aplicativos de calendário ou até mesmo em jogos que precisam registrar a data de criação do usuário. Além disso, ter a data atual também pode ajudar na organização de dados e em cálculos relacionados a prazos.

## Como obter a data atual em C++

Para obter a data atual em C++, podemos utilizar a biblioteca padrão do C++ chamada "ctime", que contém funções para manipular data e hora.

```C++
#include <iostream>
#include <ctime>

int main() {
    // Obtém a data e hora atual
    time_t currentTime = time(0);
    
    // Converte o tempo para uma representação legível
    char* date = ctime(&currentTime);

    // Imprime a data atual
    std::cout << "A data atual é: " << date << std::endl;

    return 0;
}
```

A saída do código acima será algo semelhante a:

```
A data atual é: Wed Dec 16 15:57:11 2020
```

Podemos ver que a função `ctime()` retorna uma string com a data e hora em um formato específico. É importante lembrar que o valor de `currentTime` é em segundos, então para obtermos a data e hora no formato que quisermos, precisamos usar outras funções de formatação de data e hora disponíveis na biblioteca `ctime`.

## Profundidade técnica

A função `time()` retorna o tempo atual medido em segundos a partir do dia 1º de janeiro de 1970. Esse valor é conhecido como Unix timestamp e é amplamente utilizado em programação para representar datas e horas.

Uma vez que temos esse valor, podemos usá-lo como parâmetro para outras funções, como `ctime()`, `localtime()` e `gmtime()`, para obter a data e hora em diferentes formatos.

Podemos também usar a estrutura `tm` (definida na biblioteca `ctime`) para armazenar informações específicas de data e hora, como dia, mês, ano, hora, etc.

## Veja também

- [Documentação da biblioteca ctime em C++](https://www.cplusplus.com/reference/ctime/)
- [Como utilizar timestamps em programação](https://www.unixtimestamp.com/)
- [Tutorial sobre formatação de datas e horas em C++](https://www.geeksforgeeks.org/date-time-programming-in-c-with-examples/)