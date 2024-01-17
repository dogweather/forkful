---
title:                "Cálculo de uma data no futuro ou passado"
html_title:           "C: Cálculo de uma data no futuro ou passado"
simple_title:         "Cálculo de uma data no futuro ou passado"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## O que & Por quê?
A tarefa de calcular uma data no futuro ou passado é uma função importante para programadores, pois permite a previsão de eventos e automatização de tarefas. Em linguagens de programação, essa função é realizada por diversas funções e bibliotecas, como a ```time.h```.

## Como fazer:
A função ```time.h``` permite o acesso a informações importantes de data e hora do sistema, como o dia, mês, ano, entre outros. Utilizando as funções ```localtime()``` e ```mktime()```, podemos manipular essas informações para calcular uma data no futuro ou passado.

Exemplo de código para calcular uma data no futuro:

```
#include <stdio.h>
#include <time.h>

int main()
{
   // Definindo a data atual
   time_t now = time(NULL);
   // Convertendo para uma estrutura de tempo local
   struct tm * local = localtime(&now);

   // Adicionando 10 dias à data atual
   local->tm_mday += 10;

   // Convertendo a data modificada para um timestamp
   time_t future = mktime(local);

   // Imprimindo a nova data
   printf("Data daqui a 10 dias: %s", ctime(&future));

   return 0;
}
```
Output:
```
Data daqui a 10 dias: Mon Jul 12 19:51:40 2021
```

Exemplo de código para calcular uma data no passado:

```
#include <stdio.h>
#include <time.h>

int main()
{
   // Definindo a data atual
   time_t now = time(NULL);
   // Convertendo para uma estrutura de tempo local
   struct tm * local = localtime(&now);

   // Subtraindo 5 meses da data atual
   local->tm_mon -= 5;

   // Convertendo a data modificada para um timestamp
   time_t past = mktime(local);

   // Imprimindo a nova data
   printf("Data de 5 meses atrás: %s", ctime(&past));

   return 0;
}
```
Output:
```
Data de 5 meses atrás: Wed Feb 17 19:56:16 2021
```

## Deep Dive:
A tarefa de calcular datas no futuro ou passado é uma função presente em diversas linguagens de programação e sistemas operacionais desde a década de 1960. Antes da criação de bibliotecas e funções específicas, os programadores precisavam realizar cálculos complexos envolvendo o número de dias em cada mês e anos bissextos. Hoje, com o avanço da tecnologia, essa tarefa se tornou mais simples e acessível com a utilização de funções específicas.

Além das funções ```localtime()``` e ```mktime()```, existem outras alternativas para realizar o cálculo de datas no futuro ou passado, como a biblioteca ```date.h```, que possui funções mais precisas e flexíveis.

É importante lembrar que, ao utilizar essas funções, é necessário ter cuidado com a forma como as informações de data são manipuladas, para evitar erros e problemas de compatibilidade entre sistemas.

## See Also:
- [Documentação oficial da função time.h](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [Tutorial sobre manipulação de datas em C](https://www.geeksforgeeks.org/date-manipulation-in-c/)
- [Exemplos de uso da biblioteca date.h](https://github.com/hroptatyr/date)