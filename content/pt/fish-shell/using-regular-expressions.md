---
title:                "Fish Shell: Utilizando expressões regulares"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por que usar expressões regulares na programação?

Uma das principais razões para utilizar expressões regulares na programação é para realizar buscas e manipulações de texto de forma mais eficiente e precisa. Com elas, é possível encontrar padrões específicos em um texto, substituir partes dele ou até mesmo extrair informações específicas, tornando o processo de desenvolvimento mais eficiente e rápido.

## Como usar expressões regulares no Fish Shell?

O Fish Shell oferece suporte completo para expressões regulares, permitindo que os desenvolvedores utilizem essa ferramenta poderosa durante o desenvolvimento. Abaixo, estão alguns exemplos de como utilizar expressões regulares no Fish Shell:

```
Fish Shell ~> set name "John Doe"

Fish Shell ~> echo $name
John Doe

Fish Shell ~> set name (string replace -r "Doe" "Smith" $name)

Fish Shell ~> echo $name
John Smith
```

Neste exemplo, o comando `set` é utilizado para atribuir um valor à variável `name`, que, em seguida, é exibido através do comando `echo`. Em seguida, é utilizada a função `string replace` com a opção `-r` para substituir o sobrenome "Doe" por "Smith", obtendo o resultado final de "John Smith" para a variável `name`.

## Mergulho profundo em expressões regulares

Expressões regulares são, na verdade, um padrão específico de caracteres utilizados para realizar operações de busca e manipulação em um texto. Com elas, é possível realizar operações complexas de forma mais eficiente, já que existem diversos recursos e opções que permitem tornar a busca ainda mais precisa. Alguns desses recursos incluem:

- Metacaracteres: são símbolos especiais que possuem um significado específico dentro de uma expressão regular, como o `*` que representa 0 ou mais caracteres.

- Classes de caracteres: são usadas para especificar um conjunto de caracteres que desejamos encontrar em uma busca, como `[abc]` que correspondem a qualquer uma das letras "a", "b" ou "c".

- Grupos de captura: são utilizados para identificar e armazenar partes específicas de uma expressão regular que foram encontradas durante a busca.

Além disso, existem diversos recursos e opções específicas do Fish Shell para utilizar expressões regulares de forma ainda mais avançada. Para conferir a documentação completa e se aprofundar ainda mais nesse tema, confira o manual do Fish Shell.

## Veja também

- [Documentação completa do Fish Shell](https://fishshell.com/docs/current/index.html)
- [Expressões regulares em 30 minutos](https://regexone.com/)
- [Tutorial de expressões regulares no Fish Shell](https://medium.com/@krazy1kanu/expressions-regex-on-fish-shell-2d2a7c996c4f)