---
title:                "C: Comparando duas datas"
programming_language: "C"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Por que comparar duas datas em programação?

As datas são um elemento fundamental em muitos programas e projetos de software. Comparar duas datas pode ser uma tarefa comum em algumas situações, como verificar a idade de uma pessoa ou determinar o intervalo de tempo entre duas datas. Portanto, é importante saber como comparar duas datas em linguagem C.

## Como fazer a comparação em C?

Para comparar duas datas em C, é necessário primeiro entender como as datas são representadas em um programa. Em linguagem C, as datas são armazenadas em variáveis do tipo `struct tm`, que contém informações como ano, mês, dia, entre outros. 

Para fazer a comparação, é possível utilizar as funções `difftime()` e `mktime()` da biblioteca `<time.h>`. A função `difftime()` calcula a diferença entre duas datas em segundos, enquanto a função `mktime()` cria um objeto de tempo a partir dos valores de uma `struct tm`.

```C
#include <stdio.h>
#include <time.h>

int main(void) {
  struct tm data1 = { .tm_year = 2020, .tm_mon = 10, .tm_mday = 1 }; // 01/10/2020
  struct tm data2 = { .tm_year = 2021, .tm_mon = 10, .tm_mday = 1 }; // 01/10/2021

  time_t t1 = mktime(&data1); // cria um objeto de tempo a partir da primeira data
  time_t t2 = mktime(&data2); // cria um objeto de tempo a partir da segunda data
  
  double diferenca = difftime(t2, t1); // calcula a diferença em segundos
  int dias = (int) diferenca / (24 * 60 * 60); // converte para dias
  
  printf("Entre as duas datas há %d dias de diferença.", dias);

  return 0;
}
```

O código acima calcula a diferença em dias entre duas datas e imprime o resultado. No exemplo, a diferença é de 365 dias, já que a segunda data é um ano após a primeira.

## Aprofundando mais na comparação de datas

Além das funções mencionadas acima, existem outras maneiras de comparar datas em linguagem C, como utilizando operadores lógicos e condicionais. Também é importante estar atento às particularidades de cada linguagem, já que algumas podem ter funções específicas para comparação de datas.

Além disso, é necessário ter cuidado ao comparar datas em formatos diferentes, como datas com e sem horas, ou datas em fuso horário diferente. Isso pode afetar o resultado da comparação e gerar resultados inesperados.

## Veja também

- [Documentação da biblioteca time.h em linguagem C](https://www.gnu.org/software/libc/manual/html_node/Time.html)
- [Como comparar datas em Java](https://www.devmedia.com.br/como-comparar-datas-em-java/28279)
- [Manipulando datas e horas em Python](https://dicasdepython.com.br/como-manipular-datas-e-horas-em-python/)