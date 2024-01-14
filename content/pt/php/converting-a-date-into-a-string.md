---
title:                "PHP: Convertendo uma data em uma string"
programming_language: "PHP"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por que converter uma data em uma string?

Converter uma data em uma string é uma tarefa comum em programação PHP. Isso permite que as datas sejam formatadas de maneira legível para os usuários, além de serem armazenadas ou transmitidas de forma mais fácil. Neste post, iremos explorar como realizar essa conversão e também nos aprofundar nas diferentes opções disponíveis.

## Como fazer

Para converter uma data em uma string, podemos utilizar a função `date()` do PHP. Essa função recebe dois parâmetros: o primeiro é o formato da data que desejamos obter e o segundo é o valor da data que queremos converter.

Por exemplo, se quisermos obter a data atual no formato “Dia da semana, dia de mês de ano”, podemos utilizar o seguinte código:

```
PHP
$date = date('l, j \d\e F \d\e Y');
echo $date;
```

A saída desse código seria: “domingo, 22 de agosto de 2021”. O comando `\d\e` é utilizado para inserir o texto “de”, pois as letras “d” e “e” são caracteres especiais no formato de data do PHP.

Também é possível utilizar a função `strtotime()` para converter uma data específica em timestamp antes de passá-la como parâmetro para a função `date()`. Isso pode ser útil quando precisamos trabalhar com datas em diferentes formatos ou realizar cálculos com elas.

```
PHP
$date = strtotime('2020-12-31');
echo date('M d, Y', $date);
```

A saída desse código seria: “Dec 31, 2020”.

## Aprofundando

Além dos formatos de data pré-definidos do PHP, também é possível criar formatos personalizados utilizando os caracteres especiais disponíveis. Por exemplo, o caractere “m” representa o mês com dois dígitos e o “M” representa o mês com três letras.

Também é possível passar um parâmetro opcional de idioma para a função `date()` e obter a data formatada de acordo com o idioma escolhido. Por padrão, o idioma será baseado nas configurações do servidor, mas podemos especificá-lo através do parâmetro `setlocale()`.

```
PHP
setlocale(LC_TIME, 'pt_BR');
echo date('l, d \d\e F \d\e Y');
```

A saída seria “domingo, 22 de agosto de 2021” em português brasileiro.

Lembrando que é importante sempre conferir a documentação oficial do PHP para verificar a lista completa de caracteres especiais disponíveis e suas diferentes opções.

## Veja também

- [Documentação oficial do PHP sobre a função date()](https://www.php.net/manual/pt_BR/function.date.php)
- [Documentação oficial do PHP sobre a função strtotime()](https://www.php.net/manual/pt_BR/function.strtotime.php)
- [Lista completa de caracteres especiais para formatar datas no PHP](https://www.php.net/manual/pt_BR/datetime.format.php)