---
title:                "PHP: Convertendo uma data em uma string"
simple_title:         "Convertendo uma data em uma string"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por que

Algumas vezes, em nossas aplicações PHP, precisamos exibir datas em um formato amigável para os usuários. Em vez de mostrar a data em um formato difícil de ler, podemos convertê-la em uma string mais compreensível. Neste artigo, vamos discutir como fazer isso em PHP.

## Como Fazer

Para converter uma data em uma string, podemos usar a função `date()` do PHP. Esta função aceita dois parâmetros: o formato da data desejada e o valor da data que queremos converter. Aqui está uma forma simples de usar a função `date()` para exibir a data atual em um formato de string:

```PHP
echo date('d/m/Y');
```

O código acima irá imprimir a data atual no formato DD/MM/AAAA. Mas e se quisermos exibir a data em um formato diferente? Felizmente, a função `date()` nos dá muitas opções para formatar nossas datas. Aqui estão alguns exemplos:

```PHP
// exibir a data no formato por extenso (dia, mês e ano)
echo date('l, d \d\e F \d\e Y');

// exibir a data e a hora no formato 12 horas com AM/PM
echo date('d/m/Y h:i:s A');
```

Podemos encontrar uma lista completa de caracteres de formato de data no manual do PHP.

## Mergulho Profundo

A função `date()` do PHP pode fazer muito mais do que apenas formatar datas em strings. Também podemos usar esta função para obter informações específicas sobre uma data, como o número do dia da semana ou o número do mês. Isso é especialmente útil ao trabalhar com datas em um sistema de reservas ou calendário.

Por exemplo, se quisermos obter o número da semana a partir de uma data, podemos usar o caractere `W` na função `date()`. Este caractere retorna um número de 0 a 53, indicando qual semana do ano a data está localizada. Aqui está um exemplo:

```PHP
// obter o número da semana da data atual
echo date('W');
```

Além disso, podemos usar a função `strtotime()` para converter um string em uma data no formato Unix, o qual é um formato de número de segundos desde a meia-noite do dia 1º de janeiro de 1970. Podemos, então, usar essa data Unix em conjunto com a função `date()` para formatá-la da maneira que desejarmos.

## Veja também

- [Manual do PHP sobre a função date()](https://www.php.net/manual/pt_BR/function.date.php)
- [Lista de caracteres de formato de data do PHP](https://www.php.net/manual/pt_BR/function.date.php#refsect1-function.date-parameters)
- [Manual do PHP sobre a função strtotime()](https://www.php.net/manual/pt_BR/function.strtotime.php)