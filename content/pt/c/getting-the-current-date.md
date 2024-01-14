---
title:                "C: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por que obter a data atual é importante?

A obtenção da data atual é uma tarefa importante em muitos projetos de programação. É necessário para registrar e rastrear eventos em um programa, organizar arquivos por data, gerar relatórios e muito mais. Portanto, ter a habilidade de obter a data atual em um programa pode ser útil em diversas situações.

## Como obter a data atual em um programa C?

Em C, obter a data atual é uma tarefa simples graças à biblioteca <time.h>. A função time() retorna o número de segundos desde 1 de janeiro de 1970, conhecida como a "epoch". Para obter a data atual em um formato mais legível, podemos usar a função localtime() para converter os segundos em uma estrutura de tipo tm, que representa a data e hora atuais. Veja um exemplo abaixo:

```C
#include <stdio.h>
#include <time.h>

int main() {
  time_t segundos; // declaração de variável de tipo time
  time(&segundos); // obtém os segundos atuais
  struct tm *data_hora = localtime(&segundos); // converte em uma estrutura tm
  printf("A data atual é: %02d/%02d/%d\n", data_hora->tm_mday, data_hora->tm_mon+1, data_hora->tm_year+1900); // imprime a data atual no formato DD/MM/AAAA
  return 0;
}
```

Saída:
```
A data atual é: 27/02/2021
```

## Aprofundando na obtenção da data atual em C

A função localtime() usa um fuso horário específico para converter os segundos da epoch em uma estrutura tm. Portanto, a data e hora retornadas podem ser diferentes dependendo do fuso horário da máquina. Além disso, é importante notar que a função time() retorna o número de segundos em um tipo de dados time_t, que pode variar de tamanho dependendo do sistema operacional. Portanto, pode ser necessário fazer uma conversão antes de usar em outros cálculos.

Outra função útil relacionada à obtenção da data atual em C é a strftime(), que permite a formatação da data e hora de acordo com especificações definidas pelo usuário. Seu uso pode ser visto no exemplo abaixo:

```C
#include <stdio.h>
#include <time.h>

int main() {
  char data_hora[50];
  time_t segundos;
  time(&segundos);
  struct tm *info = localtime(&segundos);
  strftime(data_hora, 50, "A data atual formatada é %d de %B de %Y, %H:%M", info);
  printf("%s\n", data_hora);
  return 0;
}
```

Saída:
```
A data atual formatada é 27 de fevereiro de 2021, 19:40
```

## Veja também

- [Documentação da biblioteca time.h em C](http://www.cplusplus.com/reference/ctime/)
- [Tutorial sobre a obtenção da data atual em C](https://www.programiz.com/c-programming/c-date-time)
- [Explicação sobre a epoch em programação](https://en.wikipedia.org/wiki/Unix_time)