---
title:                "Analisando HTML"
html_title:           "Gleam: Analisando HTML"
simple_title:         "Analisando HTML"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/parsing-html.md"
---

{{< edit_this_page >}}

## Por que
Parsing HTML pode ser uma tarefa tediosa e propensa a erros, mas com a linguagem de programação [Gleam](https://gleam.run/), é possível tornar esse processo mais simples e eficiente.

## Como fazer
Para começar a fazer o parsing de HTML com Gleam, é preciso primeiro instalar a [biblioteca de HTML](https://hex.pm/packages/gleam_html) disponível no [Hex](https://hex.pm/).

Dentro do seu código, é preciso importar essa biblioteca utilizando o comando `import html` e, em seguida, utilizar a função `parse` para passar o conteúdo HTML que deseja analisar. Por exemplo:

```
Gleam ..

import html

html.parse("<h1>Olá Gleam!</h1>")
```

O resultado deste código será uma estrutura de dados que representa o HTML fornecido, permitindo que você acesse o conteúdo de forma mais organizada e precisa.

## Aprofundando
Além de simplesmente fazer o parsing de um documento HTML, o Gleam também oferece um conjunto de funções para extrair dados específicos do documento, como tags, atributos e texto. Essas funções incluem `get_tag`, `get_attr` e `get_text`.

Além disso, o Gleam também permite a utilização de expressões XPath para selecionar elementos específicos do documento, tornando o processo de parsing ainda mais poderoso e flexível.

## Veja também
- [Documentação oficial do Gleam](https://gleam.run/documentation/)
- [Guia de instalação](https://gleam.run/getting-started/)
- [Pacote Gleam HTML no Hex](https://hex.pm/packages/gleam_html)