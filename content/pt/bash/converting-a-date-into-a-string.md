---
title:                "Bash: Convertendo uma data em uma string"
simple_title:         "Convertendo uma data em uma string"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Porque

Ao escrever scripts ou programas em Bash, pode ser necessário converter uma data em uma string. Isso é útil para exibir a data em um formato específico ou para armazená-la em um arquivo com um nome específico. Neste artigo, vamos ver como fazer isso de forma fácil e eficiente.

## Como fazer

Para converter uma data em uma string, precisamos usar o comando `date` com a opção `-d` seguida da data a ser convertida. Por exemplo, se quisermos converter a data atual, usamos `date -d "now"`.

Em seguida, para formatar a data conforme desejado, usamos a opção `-u` para usar o formato UTC, seguida pela opção `+%Y-%m-%d`, que especifica o formato de ano-mês-dia. Se quisermos adicionar o horário, podemos usar a opção `+%H-%M-%S`.

```
Bash
# Convertendo a data atual para ano-mês-dia
date -d "now" -u +%Y-%m-%d
Saída: 2019-09-12
# Adicionando o horário
date -d "now" -u +%Y-%m-%d-%H-%M-%S
Saída: 2019-09-12-12-30-00
```

A opção `date` também nos permite converter uma data específica em uma string. Por exemplo, para converter a data 20 de janeiro de 2019, usamos `date -d "2019-01-20"` e formatamos da mesma forma que antes.

## Mergulho profundo

Além das opções mencionadas, o comando `date` oferece uma ampla gama de opções de formato para personalizar a saída da data. Por exemplo, podemos usar `%j` para obter o número do dia do ano ou `%c` para obter a data e hora com o formato local.

Outra opção útil é a opção `-r`, que nos permite converter um timestamp em uma string de data. Podemos obter o timestamp atual usando o comando `date +%s`. Por exemplo, `date -r 1568293755` irá converter o timestamp 1568293755 para uma string de data correspondente.

## Veja também

- Documentação oficial do `date`: https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html
- Mais exemplos de formatação de data: https://www.cyberciti.biz/faq/linux-unix-formatting-dates-for-display/
- Tutoriais sobre Bash: https://linux.die.net/Bash