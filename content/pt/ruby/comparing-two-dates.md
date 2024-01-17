---
title:                "Comparando duas datas"
html_title:           "Ruby: Comparando duas datas"
simple_title:         "Comparando duas datas"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

## O que e por que?

Comparar duas datas é uma operação comum na programação que envolve comparar o valor de duas datas diferentes. Programadores frequentemente fazem isso para determinar qual data é mais recente ou para verificar se duas datas são iguais.

## Como fazer:

Comparar duas datas é simples com a ajuda da linguagem de programação Ruby. Veja abaixo alguns exemplos de código e as respectivas saídas:

```Ruby
# Comparando se a data 1 é maior que a data 2
data1 = Date.parse("2020-01-01")
data2 = Date.parse("2019-01-01")
data1 > data2  # => true

# Comparando se a data 1 é igual a data 2
data1 = Date.parse("2020-01-01")
data2 = Date.parse("2020-01-01")
data1 == data2  # => true
```

## Mais detalhes:

Em termos históricos, a comparação de datas é uma operação fundamental na programação. A linguagem de programação Ruby oferece várias funções e métodos que tornam esse processo mais fácil e intuitivo.

Além disso, existem diversas alternativas para comparar duas datas em Ruby, incluindo o uso da biblioteca padrão "Date", que fornece uma ampla variedade de funções e métodos para trabalhar com datas.

A comparação de datas também pode ser feita comparando os valores numéricos que representam as datas, no formato "yyyymmdd". No entanto, é importante estar atento a problemas relacionados com diferentes formatos de data em diferentes países e regiões.

Para implementar a comparação de datas em seu próprio código, basta utilizar os operadores ">", "<" ou "==", dependendo do tipo de comparação que deseja fazer. Também é possível utilizar o método "compare" da classe "Date" para obter valores numéricos (-1, 0 ou 1) que representam a relação entre duas datas.

## Veja também:

Para saber mais sobre comparação de datas em Ruby, consulte a documentação oficial da linguagem em: https://www.ruby-lang.org/pt/documentation/

Você também pode encontrar tutoriais e exemplos úteis em sites como o "Ruby Learning": http://rubylearning.com/