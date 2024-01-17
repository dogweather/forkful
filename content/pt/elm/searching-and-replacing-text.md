---
title:                "Busca e substituição de texto"
html_title:           "Elm: Busca e substituição de texto"
simple_title:         "Busca e substituição de texto"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# O que e Por que?

Substituir texto em um programa é uma tarefa comum para os programadores. Trata-se de fazer alterações em partes específicas do código, seja para corrigir erros ou para modificar seu funcionamento. Os programadores fazem isso para garantir que o código esteja funcionando corretamente e para torná-lo mais eficiente.

# Como fazer:

```Elm
texto = "Olá mundo!"
novoTexto = replace "mundo" "Elm" texto
```

Saída:
```
"Olá Elm!"
```

A função ```replace``` é utilizada para substituir uma parte de uma string por outra. No exemplo acima, "mundo" é substituído por "Elm" na string "Olá mundo!". É importante notar que a função ```replace``` não altera a string original, mas sim retorna uma nova string com as alterações feitas.

Outra forma de substituir texto em Elm é utilizando a função ```Regex.replace``` do pacote ```elm/regex```. Ela permite fazer substituições baseadas em padrões de expressões regulares, o que pode ser muito útil em casos mais complexos.

# Deep Dive:

A necessidade de substituir texto em programas vem desde os primeiros dias da programação. Antes da existência de ferramentas automatizadas, os programadores tinham que fazer as substituições manualmente, o que era um processo demorado e sujeito a erros.

Hoje, além das ferramentas nativas em linguagens de programação como Elm, existem também ferramentas externas que oferecem recursos adicionais, como busca e substituição em vários arquivos simultaneamente.

No contexto de Elm, é importante ter em mente que a função ```replace``` trata as strings como valores imutáveis, ou seja, não sofrerão alterações. Por isso, é sempre necessário atribuir o resultado da função a uma nova variável.

# Veja também:

[Documentação da função replace em Elm](https://package.elm-lang.org/packages/elm/core/latest/String#replace)

[Documentação da função Regex.replace em Elm](https://package.elm-lang.org/packages/elm/regex/latest/Regex#replace)