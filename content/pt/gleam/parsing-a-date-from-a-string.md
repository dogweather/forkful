---
title:                "Analisando uma data a partir de uma string"
html_title:           "Gleam: Analisando uma data a partir de uma string"
simple_title:         "Analisando uma data a partir de uma string"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## O que é e Por que Fazer? 
Parsing de uma data a partir de uma string é quando um programa lê uma string e a converte em uma data reconhecível pelo computador. Isso é frequentemente feito por programadores para obter informações mais precisas sobre as datas fornecidas em um código.

## Como Fazer:
Gleam possui uma função embutida para fazer parsing de datas a partir de uma string. Veja o exemplo abaixo:

```Gleam
import Gleam.Date

let data_string = "10/08/2021"
let data_parseada = Date.fromString(data_string)
```

O resultado será a data no formato de data reconhecível pelo computador. No exemplo acima, a data_parseada será ```2021-08-10```.

## Profundando na Questão:
Fazer parsing de datas a partir de strings é uma habilidade importante para programadores, pois permite que o código seja mais dinâmico e preciso. Existem outras formas de fazer isso, como utilizando bibliotecas externas ou escrevendo um algoritmo próprio. No entanto, a função embutida do Gleam torna o processo mais simples e eficiente.

## Veja Também:
Confira a documentação da função ```fromString``` para obter mais detalhes sobre como utilizar essa função no seu código. Além disso, explore outras funções do módulo Gleam Date para trabalhar com datas de forma mais eficiente.