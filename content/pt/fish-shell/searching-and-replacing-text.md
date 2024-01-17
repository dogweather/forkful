---
title:                "Buscando e substituindo texto"
html_title:           "Fish Shell: Buscando e substituindo texto"
simple_title:         "Buscando e substituindo texto"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## O que é e por que fazer isso?

Pesquisar e substituir texto é uma tarefa frequente no mundo da programação. Isso envolve encontrar uma determinada sequência de caracteres em um texto e substituí-la por outra. Programadores geralmente fazem isso para corrigir erros de digitação, alterar padrões de texto ou fazer alterações em um grande número de arquivos ao mesmo tempo.

## Como fazer:

Codificação de exemplo usando o Fish Shell:

```
# Substituindo um único caractere:
echo "Olá, mundo!" | sed 's/m/J/'

# Substituindo uma palavra inteira:
echo "Eu gosto de peixes" | sed 's/peixes/cachorros/'

# Substituindo várias ocorrências:
echo "Tenho 10 maçãs e 5 bananas" | sed 's/10/20/g'
```

Output:

```
Olá, jundo!
Eu gosto de cachorros
Tenho 20 maçãs e 5 bananas
```

## Mergulho profundo:

Historicamente, a função de pesquisa e substituição de texto foi introduzida no editor de texto Unix, chamado "ed". No entanto, com o tempo, surgiram alternativas mais avançadas, como o comando "sed" e o editor "vi". No Fish Shell, essas funções são incorporadas e oferecem mais opções, como substituições globais e expressões regulares.

## Veja também:

- [Documentação oficial do Fish Shell sobre substituições de texto](https://fishshell.com/docs/current/cmds/sed.html)
- [Artigo sobre expressões regulares no Fish Shell](https://medium.com/@mlenne/fish-shell-regular-expressions-de32477abeb4)
- [Tutorial de substituição de texto no Fish Shell](https://scriptingosx.com/fish-shell-101-pipes-and-substitutions/)