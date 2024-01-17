---
title:                "Capitalizando uma string"
html_title:           "Gleam: Capitalizando uma string"
simple_title:         "Capitalizando uma string"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# O Que & Porquê?

Capitalizar uma string significa alterar a primeira letra de cada palavra para maiúscula. Os programadores costumam fazer isso para melhorar a legibilidade e padronização do código.

# Como Fazer:

```
Gleam.Utils.to_title_case("ola, mundo") // Ola, Mundo
Gleam.Utils.to_title_case("o código é capitalizado") // O Código É Capitalizado
```

# Profundidade:

1. Contexto histórico: A prática de capitalizar strings vem da língua inglesa, onde é comum escrever nomes de pessoas, lugares e títulos de forma capitalizada.
2. Alternativas: Além de utilizar bibliotecas de terceiros, é possível criar uma função personalizada para capitalizar strings em outras linguagens de programação.
3. Detalhes de implementação: Em Gleam, é utilizada a função ```String.to_title_case/1``` que realiza a capitalização de acordo com as regras da língua inglesa.

# Veja Também:

- [Referência de Funções da Biblioteca Gleam](https://gleam.run/modules/gleam-stdlib.html#toc13)
- [Artigo sobre Padronização de Código em Gleam](https://dokku.com/blog/gleam-code-styling/)