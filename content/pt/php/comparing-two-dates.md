---
title:                "Comparando duas datas"
html_title:           "C#: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Comparar duas datas é um conceito de programação básico que verifica a diferença ou semelhança entre duas diferentes instâncias de datas. Programadores fazem isso para lidar com cenários de lógica condicional, como verificar se uma data de expiração passou ou calcular a duração entre duas datas.

## Como Fazer:

No PHP, comparar duas datas é direto. Aqui está um exemplo básico usando a classe DateTime do PHP:

```PHP
$data1 = new DateTime("2022-03-05");
$data2 = new DateTime("2022-04-05");

if ($data1 < $data2) {
    echo "A data1 é antes da data2.";
} else {
    echo "A data1 é depois da data2.";
}
```
A saída será: "A data1 é antes da data2."

## Mergulho Profundo:

Historicamente, a capacidade de comparar datas no PHP nem sempre foi tão simplificada. Antes da introdução da classe DateTime no PHP 5.2.0, os developers tiveram que se apoiar em funções de processamento de strings e timestamps UNIX.

Como alternativa à técnica acima, você pode usar a função `strcmp()` baseada em strings do PHP. No entanto, fazer isso significaria perder os poderosos métodos e propriedades que a classe DateTime oferece, e pode introduzir complicações quando se lida com fusos horários e formatos de data variados.

Finalmente, um ponto crucial na implementação: quando você compara objetos DateTime com operadores de comparação (>, <, ==), o PHP compara suas timestamps UNIX internas. Isto é, ele não está comparando as strings de data, mas sim a representação numérica em segundos dessas datas, então a comparação é extremamente rápida e eficaz.

## Veja Também:

- Documentação oficial do PHP na classe DateTime: https://www.php.net/manual/pt_BR/class.datetime.php
- Artigo no StackOverflow sobre "Como comparar datas no PHP": https://stackoverflow.com/questions/3026619/how-to-compare-dates-in-php
- Tutorial sobre a gestão de datas e tempos no PHP: https://www.tutorialspoint.com/php/php_date_and_time.htm