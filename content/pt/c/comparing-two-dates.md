---
title:                "C: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por que Comparar Duas Datas?

Comparar duas datas é uma tarefa comum na programação. Isso pode ser necessário para verificar se uma data é maior, menor ou igual a outra, ou para calcular a diferença entre duas datas. Além disso, pode ser útil para ordenar eventos em ordem cronológica, como em aplicações de calendário ou planejamento.

## Como Fazer Isso em C

Em C, podemos comparar duas datas utilizando a função `difftime()`, que calcula a diferença entre duas datas em segundos. Essa função possui a seguinte sintaxe:

```
double difftime(time_t data1, time_t data2);
```

Onde `data1` e `data2` são do tipo `time_t`, que é um tipo de dado utilizado para armazenar datas e horas em um formato específico.

Abaixo, temos um exemplo de código que compara duas datas e imprime o resultado na tela:

```
#include <stdio.h>
#include <time.h>

int main(){
  time_t data1 = time(NULL); // obtém data e hora atual
  time_t data2 = 1577836800; // 01/01/2020
  
  double diferenca = difftime(data1, data2);
  
  printf("A diferença entre as datas em segundos é: %f", diferenca);
  
  return 0;
}
```

O resultado dessa comparação será a diferença em segundos entre a data atual e o dia 01/01/2020. Podemos utilizar essa informação para fazer comparações adicionais, como verificar se uma data é maior, menor ou igual a outra.

## Profundidade da Comparação de Datas

Além de utilizar a função `difftime()`, também é possível realizar comparações de datas utilizando a struct `tm`, presente na biblioteca `time.h`. Essa struct possui campos para armazenar informações sobre o ano, mês, dia, hora, entre outros dados. Com ela, podemos comparar cada um desses campos individualmente, o que pode ser útil em determinadas situações.

Além disso, vale lembrar que a função `difftime()` considera apenas a diferença entre duas datas em segundos, mas não leva em conta informações como fusos horários ou horário de verão. Portanto, é importante ter isso em mente ao realizar comparações entre datas em diferentes regiões ou períodos do ano.

## Veja Também

- [Tutorial: Trabalhando com Datas e Horas em C](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [Documentação Oficial do C - time.h](https://www.tutorialspoint.com/c_standard_library/time_h.htm)