---
title:                "Obtendo a data atual"
date:                  2024-01-20T15:13:30.320564-07:00
html_title:           "Bash: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Obter a data atual em programação é essencialmente capturar o momento presente em que o programa está rodando. Programadores fazem isso por um monte de razões, seja para registrar eventos, marcar transações, ou simplesmente para mostrar um relógio na sua interface.

## Como Fazer:

Para pegar a data e hora atuais em C, você vai precisar mexer um pouco com as bibliotecas `time.h` e `stdio.h`. Aqui vai um exemplo simples:

```C
#include <stdio.h>
#include <time.h>

int main() {
    time_t tempo_atual;
    struct tm *info_tempo;

    time(&tempo_atual); // Pega o tempo atual
    info_tempo = localtime(&tempo_atual); // Converte para o horário local

    // Imprime a data e hora no formato AAAA-MM-DD HH:MM:SS
    printf("Data e hora atuais: %d-%02d-%02d %02d:%02d:%02d\n", 
           info_tempo->tm_year + 1900, info_tempo->tm_mon + 1, info_tempo->tm_mday,
           info_tempo->tm_hour, info_tempo->tm_min, info_tempo->tm_sec);

    return 0;
}
```

Exemplo de saída:

```
Data e hora atuais: 2023-03-15 21:45:12
```

## Mergulho Profundo

Antigamente, a função `time` era suficiente para a maioria das aplicações, mas conforme os programas se tornavam mais globalizados, problemas com zonas horárias e horário de verão começaram a aparecer. A `struct tm` ajuda a lidar com isso.

Como alternativas, você tem bibliotecas de terceiros como a `date.h` em C++ e APIs em sistemas operacionais específicos que podem oferecer mais funcionalidades. Em nível de implementação, a função `time` geralmente pega a contagem de segundos desde o "Epoch" (vulgo: primeiro segundo de 1970) e converte em uma estrutura mais legível para humanos com `localtime` ou `gmtime`.

## Veja Também

- Documentação oficial do GNU C Library para funções de tempo (em inglês): https://www.gnu.org/software/libc/manual/html_node/Date-and-Time.html
- Tutorial sobre manipulação de data e hora em C (em inglês): https://www.tutorialspoint.com/c_standard_library/c_function_localtime.htm
- Referência da biblioteca `time.h` (em inglês): https://en.cppreference.com/w/c/chrono