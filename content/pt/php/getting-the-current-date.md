---
title:    "PHP: Obtendo a data atual"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por que obter a data atual?

Se você é um programador iniciante ou até mesmo experiente, provavelmente já teve que lidar com a obtenção da data atual em seus projetos de programação. Mas você já se perguntou por que isso é importante ou necessário? Bem, vamos explorar um pouco mais esse assunto.

## Como obter a data atual em PHP

Em PHP, obter a data atual é uma tarefa bastante simples. Para isso, utilizamos a função `date()` que aceita dois parâmetros: o formato da data que desejamos obter e um timestamp opcional. Por exemplo, se quisermos exibir a data atual no formato "dia/mês/ano", basta utilizar o seguinte código:

```php
echo date("d/m/Y");
```

Isso irá imprimir a data atual no formato "dia/mês/ano", como por exemplo "02/08/2021". Mas e se quisermos exibir também a hora atual? Para isso, basta adicionar o formato de hora no primeiro parâmetro da função `date()`, ficando assim:

```php
echo date("d/m/Y H:i:s");
```

Isso irá imprimir a data e hora atual no formato "dia/mês/ano hora:minuto:segundo", como por exemplo "02/08/2021 10:30:15". E se quisermos exibir a data e hora de uma determinada timezone? Podemos utilizar a função `date_default_timezone_set()` para definir a timezone desejada antes de chamar a função `date()`. Por exemplo:

```php
date_default_timezone_set("America/Sao_Paulo");
echo date("d/m/Y H:i:s");
```

Isso irá imprimir a data e hora atual no formato da timezone definida, como por exemplo "02/08/2021 10:30:15" para a timezone de São Paulo.

## Profundando mais na obtenção da data atual

Além da função `date()`, o PHP também possui outras funções para obter a data atual, como por exemplo as funções `time()` e `strtotime()`. A função `time()` retorna um timestamp UNIX (o número de segundos desde 1 de janeiro de 1970 00:00:00 GMT), enquanto a função `strtotime()` converte uma representação de data e hora em timestamp UNIX.

Outra possibilidade é utilizar a classe `DateTime` para manipular a data e hora de forma mais precisa, permitindo até mesmo fazer cálculos e comparações entre datas.

Em resumo, obter a data atual em PHP é uma tarefa muito simples, mas é importante entender as diferentes formas de fazê-lo e escolher a que melhor se adapta ao seu projeto.

## Veja também

- [Documentação oficial do PHP sobre a função date()](https://www.php.net/manual/pt_BR/function.date.php)
- [Documentação oficial do PHP sobre a classe DateTime](https://www.php.net/manual/pt_BR/class.datetime.php)
- [Artigo sobre manipulação de datas e horas em PHP](https://blog.usedev.com.br/manipulando-datas-no-php/)