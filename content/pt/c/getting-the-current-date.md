---
title:                "Obtendo a data atual"
html_title:           "C: Obtendo a data atual"
simple_title:         "Obtendo a data atual"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## O que & Porquê?
Obter a data atual é uma tarefa comum em programação que consiste em obter a data e hora no momento em que o código está sendo executado. Isso é feito para registrar as informações em sistemas de registro, gerar relatórios em tempo real ou para executar alguma lógica de negócios baseada em data e hora.

## Como:
Usar a função `time()` da biblioteca padrão `time.h` é a maneira mais simples de obter a data atual em um programa C. Ela retorna o número de segundos desde 1º de janeiro de 1970, também conhecido como o "Unix epoch". Para obter a data atual, basta dividir esse valor pelo número de segundos em um dia (86400). O código abaixo mostra um exemplo deste processo:
```
#include <stdio.h>
#include <time.h>

int main() {
  time_t current_time; // tipo de dados para armazenar o valor retornado por time()
  int seconds_in_a_day = 86400; // número de segundos em um dia
  int current_date; // variável para armazenar a data atual
  
  current_time = time(NULL); // obtemos o valor retornado por time()
  current_date = current_time / seconds_in_a_day; // dividimos pelo número de segundos em um dia para obter a data
  
  printf("A data atual é: %d\n", current_date);
  
  return 0;
}
```
A saída deste código seria algo como:
```
A data atual é: 18654
```
Este valor representa o número de dias desde 1º de janeiro de 1970 até a data atual.

## Deep Dive:
A obtenção da data atual é uma funcionalidade essencial para qualquer programa que lide com data e hora. Antigamente, era comum que cada sistema operacional tivesse sua própria maneira de obter a data atual, o que dificultava a portabilidade de programas entre diferentes plataformas. No entanto, com a introdução do padrão ANSI C em 1989, a função `time()` se tornou parte da biblioteca padrão `time.h`, tornando mais fácil e consistente a obtenção da data atual em diferentes sistemas operacionais.

Apesar de a função `time()` ser a maneira mais comum de obter a data atual em programas C, existem outras alternativas, como a função `localtime()` que permite obter a data atual em um formato mais legível, ou mesmo bibliotecas de terceiros que podem oferecer funcionalidades mais avançadas.

Além disso, é importante mencionar que a obtenção da data atual não é uma tarefa trivial, uma vez que está sujeita a mudanças de fuso horário e datas incorretas definidas pelo usuário no sistema operacional. Por isso, é essencial que programadores lidem com essas situações de forma adequada em seus códigos.

## See Also:
- [Documentação da função time() na biblioteca padrão C](https://www.cplusplus.com/reference/time/time/)
- [Explicação sobre o Unix epoch](https://www.epochconverter.com/)