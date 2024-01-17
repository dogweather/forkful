---
title:                "Encontrando o comprimento de uma string"
html_title:           "Elm: Encontrando o comprimento de uma string"
simple_title:         "Encontrando o comprimento de uma string"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

O que & Porquê?

Encontrar o comprimento de uma string é um processo comum na programação que envolve determinar o número de caracteres em uma string. Isso pode ser útil para diversas tarefas, como validação de input do usuário, manipulação de textos e formatação de dados.

Como fazer:

Para encontrar o comprimento de uma string em Elm, podemos usar a função `String.length` que retorna o número de caracteres na string fornecida. Por exemplo:

```Elm
String.length "Olá mundo!" -- retorna 10
```
Podemos também usar essa função com variáveis e inputs de usuário:

```Elm
nome <- "Maria"
String.length nome -- retorna 5
```

Deep Dive:

A necessidade de encontrar o comprimento de uma string vem desde os primeiros dias da programação, quando a manipulação de textos era uma tarefa essencial. Além disso, essa função também pode ser implementada de maneira recursiva, percorrendo a string e contando cada caractere até alcançar o final.

Alternativamente, também é possível usar a função `List.length` em combinação com `String.toList` para obter o tamanho de uma string em forma de lista de caracteres.

Ver também:

- Documentação oficial do Elm sobre a função `String.length`: https://package.elm-lang.org/packages/elm/core/latest/String#length
- Outras funções úteis de string em Elm: https://guide.elm-lang.org/strings/
- Exemplos práticos de uso da função `length`: https://dev.to/larsenwork/elm-recipes-string-length-3goe