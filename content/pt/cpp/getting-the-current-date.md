---
title:                "Obtendo a data atual"
html_title:           "C++: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

# O que é e por que fazer a obtenção da data atual?

A obtenção da data atual é um processo simples em que um programa de computador obtém a data atual do sistema operacional e a utiliza para realizar determinadas tarefas. Programadores geralmente fazem isso para registrar informações em logs ou para exibir a data atual em um programa.

# Como fazer:

```
#include <iostream>
#include <ctime>

using namespace std;

int main()
{
    // Obtém a data atual do sistema
    time_t now = time(0);

    // Converte a data para o formato de string
    char* dt = ctime(&now);

    // Imprime a data atual
    cout << "A data atual é: " << dt << endl;

    return 0;
}
```

Saída:
A data atual é: Ter Dec 14 08:54:08 2021

# Deep Dive

## Contexto histórico:

Antes do padrão C++11, a biblioteca `<ctime>` não possuía funções específicas para obter a data atual. Programadores precisavam recorrer a bibliotecas de terceiros ou utilizar funções do sistema operacional, o que tornava o processo mais complexo.

## Alternativas:

Além do exemplo apresentado, existem outras maneiras de obter a data atual em um programa. Uma alternativa seria utilizar a biblioteca `<chrono>` do C++11, que possui funções mais robustas para lidar com datas e horários.

Outra opção seria utilizar uma biblioteca externa, como o Boost Date Time, que oferece uma ampla gama de funcionalidades para trabalhar com datas e horários.

# Veja também:

- [Referência da biblioteca ctime do C++](https://www.cplusplus.com/reference/ctime/)
- [Documentação da biblioteca chrono do C++11](https://en.cppreference.com/w/cpp/chrono)
- [Boost Date Time Library](https://www.boost.org/doc/libs/1_77_0/doc/html/date_time.html)