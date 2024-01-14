---
title:                "Bash: Excluindo caracteres que correspondem a um padrão"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que

O Bash é uma linguagem de script amplamente utilizada em sistemas operacionais Linux e Unix. Uma das tarefas mais comuns ao escrever scripts é manipular strings e substituir caracteres indesejados. A função "tr" do Bash é uma ferramenta poderosa para isso, permitindo que você delete caracteres que correspondam a um determinado padrão em uma string.

## Como Fazer

Para usar a função "tr" para excluir caractéres, comece escrevendo o comando `tr`, seguido pelo conjunto de caracteres que deseja substituir. Por exemplo, se você quiser excluir todos os caracteres numéricos de uma string, usaria o comando `tr 0-9`. Em seguida, use o argumento "d" para indicar que você deseja excluir os caracteres (exemplo: `tr 0-9 d`).

Por exemplo, se você tiver a string "Bash 101", o comando `echo "Bash 101" | tr 0-9 d` resultará na saída "Bash".

Se você quiser excluir mais de uma letra ou número específico, pode usar o argumento "c" (complemento) para excluir os caracteres que não deseja. Por exemplo, `tr -c aeiou` excluirá todas as vogais de uma string.

## Profundidade

A função "tr" do Bash tem muitas outras opções que podem ser usadas para manipular strings de forma mais avançada. Por exemplo, você pode usar o comando `tr` para converter caracteres em maiúsculos para minúsculos (ou vice-versa) usando o argumento "u" ou "l". Você também pode usar a opção `-s` para comprimir caracteres repetidos em uma string.

É importante ter em mente que a função "tr" funcionará de maneira diferente em sistemas operacionais diferentes. Certifique-se de verificar a documentação específica para o seu sistema antes de usar a função "tr" em seus scripts.

## Veja também

- [Documentação do Bash](https://www.gnu.org/software/bash/manual/bash.html)
- [Tutorial de Introdução ao Bash](https://linuxize.com/post/bash-scripting-tutorial/)
- [Documentação do comando "tr"](https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html)