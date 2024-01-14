---
title:                "Gleam: Maiúsculas em uma string"
simple_title:         "Maiúsculas em uma string"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por que

Capitalizar uma string é necessário em alguns casos, como por exemplo, em formulários onde é importante que os dados estejam padronizados ou em títulos de página.

## Como Fazer

Para capitalizar uma string em Gleam, nós utilizamos a função `String.capitalize/1`. Veja o exemplo abaixo:

```
Gleam> String.capitalize("gleam programming")
"Gleam programming"
```

Neste exemplo, a nossa string "gleam programming" foi capitalizada e agora está retornando "Gleam programming". Outra forma de capitalizar uma string é utilizar o operador `^`, como mostrado no exemplo abaixo:

```
Gleam> "gleam programming" ^cap
"Gleam programming"
```

Este operador utiliza a primeira letra da string como "cabeçalho" e capitaliza a mesma.

## Deep Dive

Ao capitalizar uma string em Gleam, é importante notar que a função `String.capitalize/1` é case-sensitive. Isso significa que se a primeira letra da string já estiver em maiúsculo, a função não irá alterar a string. Além disso, quando a string contém acentos, a função também não irá alterar a letra acentuada. Por exemplo:

```
Gleam> String.capitalize("árvore")
"Árvore"
```

## Veja Também

- Função String.capitalize/1 (https://gleam.run/modules/string.html#capitalize)
- Operador `^` (https://gleam.run/guides/expressions.html#prefix-operators)
- Outras funções relacionadas à manipulação de strings (https://gleam.run/modules/string.html)