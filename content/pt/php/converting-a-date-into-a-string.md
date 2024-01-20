---
title:                "Convertendo uma data em uma string"
html_title:           "C++: Convertendo uma data em uma string"
simple_title:         "Convertendo uma data em uma string"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

A conversão de uma data em uma string de caracteres permite que programadores transformem informações de data e hora em um formato legível e personalizável. Essa prática é essencial para a representação adequada de datas em diversas interfaces e formatos.

## Como Fazer:

Aqui estão alguns exemplos de como converter uma data em string usando PHP:

```PHP 
$data = new DateTime();   // Obtendo a data atual
echo $data->format('Y-m-d');  // Convertendo e exibindo a data no formato Ano-Mês-Dia.

// A saída será algo como "2022-04-21"
```
Você pode usar diversos formatos de acordo com suas necessidades. Aqui está outro exemplo:

```PHP 
$data = new DateTime();
echo $data->format('d/m/Y H:i:s');

// A saída será algo como "21/04/2022 12:34:56"
```

## Aprofundando o Assunto

A funcionalidade para formatar uma data em uma string foi introduzida no PHP 5.2.0, como parte da extensão DateTime. Anteriormente, tinhamos que fazer uso da função date() e a função strtotime() para alcançar um resultado semelhante.

Como alternativa, a função strftime() também permite converter uma data em string, oferecendo um controle mais granular da localização.

Sobre a implementação, essa funcionalidade segue o padrão DateTime::format do PHP e os formatos suportados são aqueles definidos pela função date().

## Veja Também

Abaixo estão alguns recursos úteis se você quiser se aprofundar ainda mais neste tópico.

1. Documentação oficial do PHP em: [Datetime::format](http://php.net/manual/pt_BR/datetime.format.php)
2. Para formatos de data e hora suportados, consulte: [Date Formats](https://www.php.net/manual/pt_BR/datetime.format.php)
3. Artigo sobre a função strftime(): [strftime()](https://www.php.net/manual/pt_BR/function.strftime.php)