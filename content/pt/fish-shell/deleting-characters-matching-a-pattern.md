---
title:                "Fish Shell: Excluindo caracteres que correspondem a um padrão"
simple_title:         "Excluindo caracteres que correspondem a um padrão"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que usar a função de exclusão de caracteres que correspondem a um padrão?

Engajar na exclusão de caracteres que correspondem a um padrão pode ser útil para limpar e organizar dados em um programa de computador ou para realizar tarefas específicas de processamento de texto.

## Como usar a função de exclusão de caracteres que correspondem a um padrão no Fish Shell

```Fish Shell
set texto "Olá, isso é um exemplo de texto."
echo $texto
# Saída: Olá, isso é um exemplo de texto.

set texto (delete -r oi $texto)
echo $texto
# Saída: Olá, é um exemplo de texto.
```

A função "delete" no Fish Shell permite a exclusão de uma sequência de caracteres de uma variável, indicando o padrão de caractere ou expressão regular que se deseja excluir. Ao usar a opção "-r", é possível realizar a exclusão em todo o texto, em vez de apenas na primeira ocorrência.

## Deep Dive: Mais informações sobre a exclusão de caracteres que correspondem a um padrão

A função "delete" no Fish Shell oferece a opção de usar expressões regulares ao invés de um padrão de caractere específico para realizar a exclusão. Isso permite uma maior flexibilidade na manipulação de texto, possibilitando a exclusão de caracteres baseado em padrões mais complexos.

Por exemplo, ao usar o comando ```set texto "Ele comprou 3 maçãs"```, podemos usar a função de exclusão "delete 'ma?' $texto" para eliminar as palavras "maçãs" e "ma" (considerando apenas o "a" opcional) do texto. A saída seria "Ele comprou 3 s".

Além disso, a opção "-p" pode ser usada para indicar a posição do caractere que deve ser excluído, possibilitando a remoção de caracteres específicos em uma posição determinada.

## Veja também

- [Documentação oficial do Fish Shell](https://fishshell.com/docs/current/cmds/set.html#set-a-variable-as-oli)
- [Tutorial do Fish Shell](https://fishshell.com/docs/current/tutorial.html)
- [Guia de expressões regulares no Fish Shell](https://fishshell.com/docs/current/tutorial.html#regular-expressions)