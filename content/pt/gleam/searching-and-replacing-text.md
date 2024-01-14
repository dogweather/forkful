---
title:    "Gleam: Buscando e substituindo texto"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que
Há muitas razões pelas quais alguém pode querer realizar a tarefa de busca e substituição de texto. Pode ser por correção de erros ortográficos, padronização de um código ou simplesmente para economizar tempo ao editar um documento extenso. Felizmente, no Gleam há uma maneira fácil e eficiente de realizar essa tarefa.

## Como fazer
Para buscar e substituir texto em Gleam, usamos a função `String.replace` passando a string de origem, o texto a ser substituído e o texto de substituição como argumentos. Vejamos um exemplo:

```Gleam
let texto = "Eu amo programação em Gleam."
let novoTexto = String.replace(texto, "amo", "adoro")

// Output: "Eu adoro programação em Gleam."
```

Neste exemplo, estamos substituindo a palavra "amo" por "adoro" na string de origem "Eu amo programação em Gleam". O resultado é a string modificada "Eu adoro programação em Gleam".

Podemos usar a função `String.replace` em conjunto com outras funções, como `String.split`, para tornar a busca e substituição ainda mais poderosa. No exemplo abaixo, estamos substituindo todas as vogais maiúsculas por minúsculas em uma string:

```Gleam
let texto = "ESte é um PROBLEMA"
let novoTexto = texto
  |> String.split("")
  |> List.map(String.to_lower)
  |> String.join("")

// Output: "Este é um problema"
```

Neste exemplo, primeiro dividimos a string em uma lista de caracteres, usamos a função `String.to_lower` para converter todas as letras em minúsculas e, em seguida, juntamos novamente em uma string usando a função `String.join`.

## Mergulho profundo
Além da função `String.replace`, o Gleam oferece outras funções úteis para busca e substituição de texto, como `String.replace_all` que substitui todas as ocorrências de um texto em uma string, e `String.replace_nth` que substitui apenas a enésima ocorrência.

Outra ferramenta útil é o uso de expressões regulares para buscar e substituir padrões de texto mais complexos. O Gleam tem um módulo de expressões regulares que pode ser importado e usado para realizar essas tarefas.

## Veja também
- Documentação oficial do Gleam sobre busca e substituição de texto: https://gleam.run/documentation/stdlib/string.html#replace
- Última versão do Gleam disponível no GitHub: https://github.com/gleam-lang/gleam/releases
- Comunidade Gleam no Discord: https://discord.gg/hGJGtKQsJK