---
title:                "Convertendo uma string para letra minúscula"
html_title:           "Gleam: Convertendo uma string para letra minúscula"
simple_title:         "Convertendo uma string para letra minúscula"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por que

Se você já se deparou com a necessidade de converter uma string para letras minúsculas em um projeto de programação, então você sabe o quão importante essa habilidade pode ser. É especialmente útil ao lidar com dados de entrada de usuários, pois garante que o seu programa os manipule corretamente, independentemente de como eles digitaram o texto.

## Como fazer

Aqui está um exemplo simples de como converter uma string para letras minúsculas em Gleam:

```
Gleam
  .String
  .to_lower("TEXTO EM CAIXA ALTA")
```

Isso produz a saída esperada de `texto em caixa alta`. Além disso, Gleam também fornece a função `String.to_lower_ascii`, que remove acentos e caracteres especiais antes de converter para minúsculas.

```
Gleam
  .String
  .to_lower_ascii("Olá Mundo! Como vai você?")
```

A saída será `ola mundo! como vai voce?`

## Mergulho profundo

Você pode estar se perguntando, por que precisamos de uma função específica para converter uma string para letras minúsculas? Não poderia ser feito com uma simples manipulação de caracteres?

A resposta é sim, mas essa função em Gleam garante que a conversão aconteça de forma eficiente e precisa, levando em consideração as diferenças entre os caracteres em diferentes idiomas. Além disso, essa função é imutável, o que significa que ela não altera a string original, mas sim retorna uma nova string com as letras em minúsculo.

Essa função também lida com casos especiais, como a letra "i" minúscula em turco, que é representada como "ı" e não "i" como em inglês. Portanto, se você estiver trabalhando com strings de diferentes idiomas, essa função garantirá que a conversão para minúsculo seja feita corretamente.

## Veja também

- Documentação oficial da função String.to_lower em Gleam: https://gleam.run/docs/stdlib/String#to_lower
- Tutorial de Gleam em português: https://dev.to/gleam/guia-de-instalacao-de-gleam-para-iniciantes-5chi (em inglês)