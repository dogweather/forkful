---
title:                "Excluindo caracteres que correspondem a um padrão"
html_title:           "Gleam: Excluindo caracteres que correspondem a um padrão"
simple_title:         "Excluindo caracteres que correspondem a um padrão"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Por que Deletar Caracteres Correspondentes a um Padrão no Gleam?

Você pode encontrar a necessidade de deletar caracteres correspondentes a um padrão em situações em que deseja limpar uma string de caracteres específicos, como espaços em branco ou símbolos indesejados.

## Como Fazer:

Usando o poderoso recurso de "Pattern Matching" (correspondência de padrão) do Gleam, é fácil e eficiente deletar caracteres correspondentes a um padrão. Veja um exemplo simples abaixo:

```
let string_com_espacos = "Este é um exemplo com espaços em branco"

let string_sem_espacos = "Este é um exemplo com espaços em branco"
|> String.replace(~pattern=" ", "", ~replacement="")
```

O código acima usou o `String.replace` para substituir todos os espaços em branco na string com uma string vazia, resultando em "Esteéumexemplocomespaçosembranco" na nova string `string_sem_espacos`.

Você também pode usar a sintaxe de correspondência de padrão diretamente no `let` para evitar a necessidade de criar uma nova variável:

```
let nova_string = let "Este é um exemplo com espaços em branco"
|> String.replace(~pattern=" ", "", ~replacement="")

// "Esteéumexemplocomespaçosembranco"
```

O mesmo conceito pode ser aplicado a outros padrões, como símbolos ou caracteres especiais. Basta alterar o padrão e o texto de substituição de acordo com a sua necessidade.

## Mergulho Profundo:

A correspondência de padrão é uma técnica poderosa do Gleam que permite a manipulação eficiente de strings. Além de substituir, você também pode usar `String.replace` para remover caracteres correspondentes a um padrão, adicionando mais uma barra vertical `|` após o padrão. Por exemplo, `String.replace(~pattern="a|e|i|o|u", "", ~replacement="")` irá remover todas as vogais de uma string.

Outros recursos úteis do Gleam para manipulação de strings incluem `String.split` e `String.join` para dividir e unir strings usando um separador, e `String.slice` para cortar uma string em uma posição inicial e final específica.

# Veja Também:

- [Documentação Oficial do Gleam](https://gleam.run/)
- [Tutorial do Gleam para Iniciantes](https://medium.com/@gleamlang/introduction-to-gleam-74f1f08e50fa?source=---------5------------------)
- [Exemplos de Códigos do Gleam](https://github.com/gleam-lang/gleam/tree/master/examples)