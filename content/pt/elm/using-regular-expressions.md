---
title:                "Elm: Utilizando expressões regulares"
simple_title:         "Utilizando expressões regulares"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Por que usar expressões regulares no Elm?

Expressões regulares são ferramentas poderosas para trabalhar com texto em qualquer linguagem de programação. No Elm, elas nos permitem fazer buscas e manipulações complexas de maneira rápida e eficiente. Além disso, também ajudam a tornar nosso código mais conciso e legível.

# Como usar expressões regulares no Elm

Para utilizar expressões regulares no Elm, precisamos importar o módulo `Regex` e construir um objeto Regex com a expressão que desejamos buscar. Por exemplo, se quisermos encontrar todas as ocorrências da palavra "Olá" em uma string, poderíamos fazer o seguinte:

```elm
import Regex

string = "Olá, como vai você?"

regex = Regex.regex "Olá"

matches = Regex.find regex string

-- output:
-- Just [{ match = "Olá", index = 0, number = 1 }]
```

O objeto `matches` contém as informações sobre cada ocorrência encontrada, incluindo a string correspondente, o índice de onde ela foi encontrada e o número da ocorrência. Podemos então usar essas informações para realizar manipulações ou validações específicas em nosso texto.

# Aprofundando nas expressões regulares no Elm

Além da função `find`, o módulo `Regex` também possui outras funções úteis, como `replace` para substituir uma ocorrência por outra, `contains` para verificar se a string contém alguma ocorrência ou `matches` para retornar todas as ocorrências em uma lista. Além disso, é possível utilizar caracteres especiais e classes para tornar nossas expressões regulares ainda mais poderosas.

Outra dica importante é que podemos melhorar a performance de nossas expressões regulares no Elm utilizando o operador `=>` ao invés de `$` para fazer substituições. Isso evita que a expressão seja reconstruída a cada iteração e pode ser bastante útil em casos de grande volume de dados.

# Veja também

- Documentação oficial do módulo Regex no Elm: https://package.elm-lang.org/packages/elm/regex/latest
- Artigo sobre expressões regulares no Elm do blog mobiarch: https://mobiarch.wordpress.com/2017/06/03/regular-expressions-in-elm/