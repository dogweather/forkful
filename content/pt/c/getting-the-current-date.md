---
title:                "C: Obtendo a data atual."
programming_language: "C"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por que obter a data atual em programas C

Se você é um programador C, provavelmente já precisou obter a data atual em suas aplicações. Afinal, a data é um elemento importante em muitas tarefas, como registros de eventos, agendamento e controle de versão.

## Como obter a data atual em C

Em C, existem algumas maneiras de obter a data atual, mas a mais comum é usando a função ```time()```. Esta função retorna o tempo atual em segundos desde 1º de janeiro de 1970.

Aqui está um exemplo simples de como usar a função ```time()``` para obter a data atual:

```C
#include <stdio.h>
#include <time.h>

int main()
{
    // Declara uma variável para armazenar o tempo atual
    time_t t;

    // Chama a função time() para atribuir o tempo atual à variável t
    t = time(NULL);

    // Imprime o tempo atual convertido para uma string legível
    printf("Data atual: %s", asctime(localtime(&t)));

    return 0;
}

```

O programa acima usa a função ```localtime()``` para converter o valor retornado pela função ```time()``` em uma estrutura de dados que contém informações detalhadas sobre a data e hora atual. A função ```asctime()``` então converte essa estrutura em uma string legível para ser impressa na tela.

O código acima não é à prova de falhas, pois depende do sistema operacional e do fuso horário do usuário. No entanto, é um bom exemplo para entender o conceito de obter a data atual em C.

## Aprofundando no assunto

Se você quiser usar a data atual para tarefas mais complexas, como cálculos de tempo, é importante entender como a função ```time()``` funciona.

Em poucas palavras, a função ```time()``` retorna o tempo atual em segundos. Isso é possível graças a um relógio interno no sistema conhecido como "época". A época é um momento específico na história que é usado como referência para outras datas e tempos. Em sistemas Unix, como Linux e MacOS, a época padrão é 1º de janeiro de 1970.

Isso significa que todos os valores retornados pela função ```time()``` estão em relação à época padrão. Portanto, para obter uma data específica, é necessário converter o número de segundos em uma estrutura de dados que contenha informações sobre o dia, mês, ano e hora.

## Veja também

- [Documentação da função ```time()```](https://www.tutorialspoint.com/c_standard_library/c_function_time.htm)
- [Guia completo sobre datas e horas em C](https://www.programiz.com/c-programming/c-datetime)
- [Tutorial sobre como formatar datas e horas em C](https://www.guru99.com/c-date-time.html)