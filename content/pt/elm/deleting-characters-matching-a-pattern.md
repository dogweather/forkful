---
title:                "Elm: Excluindo caracteres correspondentes a um padrão"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Por que deletar caracteres que correspondem a um padrão?

Às vezes, ao trabalhar com dados, pode ser necessário remover caracteres que correspondem a um determinado padrão. Isso pode ser feito de forma fácil e eficiente usando a linguagem de programação Elm. Neste artigo, vamos explorar como realizar essa tarefa e mergulhar mais fundo para entender como o processo funciona.

# Como fazer

Para deletar caracteres que correspondem a um padrão em Elm, primeiro precisamos importar o módulo String. Em seguida, usamos a função `replace` para substituir os caracteres desejados pelo valor vazio ("").

```
Elm import String

texto = "Olá, *mundo*!"
novoTexto = replace "*" "" texto

-- novoTexto será "Olá, mundo!"
```

No exemplo acima, usamos a função `replace` para substituir o asterisco "*" pelo valor vazio, resultando em um texto sem a formatação em negrito.

# Mergulho Profundo

Ao trabalhar com a função `replace`, é importante entender que ela só substitui a primeira ocorrência do caractere desejado. Portanto, se houver mais de uma ocorrência do caractere, precisamos usar um laço `while` para substituir todas elas.

```
Elm import String

texto = "Eu gosto de *café* com *leite*"
novoTexto = replace "*" "" texto

enquanto ((indexOf "*") /= -1) novoTexto do
    novoTexto = replace "*" "" novoTexto
```

Neste exemplo, usamos um laço `while` para substituir todas as ocorrências do caractere "*". No primeiro ciclo, apenas o primeiro "*" será substituído e, no segundo ciclo, a próxima ocorrência será substituída e assim por diante até que todas as ocorrências sejam substituídas.

# Veja também

- [Documentação oficial do módulo String em Elm](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Outros exemplos de manipulação de strings em Elm](https://elmprogramming.com/elm-string.html)
- [Como usar a função `replace` em Elm](https://www.eg.bucknell.edu/$PHP/~mead/teaching/cs206/s14-1763.html)