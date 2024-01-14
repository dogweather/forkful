---
title:    "C: Calculando uma data no futuro ou passado"
keywords: ["C"]
---

{{< edit_this_page >}}

## Por que?
Calcular a data no futuro ou passado pode ser útil em muitas situações, desde a criação de agendas programadas até a simulação de datas de pagamento ou vencimento de contas.

## Como fazer:
Para calcular uma data no futuro ou passado em C, podemos utilizar a função `mktime` da biblioteca `<time.h>`. Esta função recebe uma estrutura `tm` com as informações da data desejada e retorna o número de segundos desde 1º de janeiro de 1970 até a data especificada. Com isso, podemos converter o resultado para uma data no formato desejado utilizando a função `localtime` e imprimir o resultado.

```C
#include <stdio.h>
#include <time.h>

int main(){
    // Criando uma estrutura tm com a data atual
    struct tm dataAtual;
    time_t segundos;
    time(&segundos);
    dataAtual = *localtime(&segundos);

    // Adicionando 30 dias à data atual
    dataAtual.tm_mday += 30;
    segundos = mktime(&dataAtual);

    // Convertendo de segundos para data e imprimindo o resultado
    struct tm dataFutura = *localtime(&segundos);
    printf("A data daqui a 30 dias será: %d/%d/%d", dataFutura.tm_mday, dataFutura.tm_mon + 1, dataFutura.tm_year + 1900);

    return 0;
}
```

A saída deste código será: `A data daqui a 30 dias será: 19/4/2021`.

## Mergulho Profundo:
Além da função `mktime`, existem outras funções da biblioteca `<time.h>` que podem ser úteis para efetuar cálculos de datas. Por exemplo, a função `mktime` recebe informações de data e hora em formato UTC, mas podemos utilizar a estrutura `tm` para especificar uma data e hora no fuso horário local. Além disso, existem funções para adicionar ou subtrair dias, meses e anos de uma data, além de diferentes formas de obter informações sobre o dia da semana ou a diferença entre duas datas.

## Veja também:
- [Tutorial sobre `<time.h>` em C](https://www.programiz.com/c-programming/library-function/time.h)
- [Documentação completa da biblioteca `<time.h>`](https://en.cppreference.com/w/c/chrono)

Cálculos envolvendo datas podem ser úteis em diversas aplicações, por isso é importante entender como realizar essas operações em C. Esperamos que este artigo tenha sido útil para você :)