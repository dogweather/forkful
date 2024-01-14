---
title:    "Fish Shell: Excluindo caracteres que correspondem a um padrão"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que
Uma das tarefas mais comuns ao lidar com texto é encontrar e substituir certos padrões de caracteres. No Fish Shell, existe uma função ampla e útil para fazer exatamente isso: a exclusão de caracteres que correspondem a um padrão. Neste post, vamos explorar por que você pode querer usar essa função e como fazê-la em seu código.

## Como Fazer
Para excluir caracteres correspondentes a um padrão no Fish Shell, você pode usar o operador `~=` (tilde igual) seguido do padrão entre aspas duplas. Por exemplo, se quisermos excluir todas as vogais de uma string, podemos usar o seguinte código:

```
set texto "Olá Mundo"
set novo_texto $texto[~="aeiou"]
echo $novo_texto
```
Isso imprimirá "Ol Mnd" na tela. O operador `~=` irá percorrer toda a string e excluir qualquer caractere que corresponda ao padrão "aeiou".

Mas o que acontece se você desejar excluir apenas algumas letras e não todas? É aí que entra o operador `!~=` (tilde exclamação igual). Este operador exclui todos os caracteres que não correspondem ao padrão. Por exemplo, se quisermos excluir todas as letras maiúsculas em uma string, podemos usar o seguinte código:

```
set texto "Olá Mundo"
set novo_texto $texto[!~="A-Z"]
echo $novo_texto
```
Isso imprimirá "lá undo" na tela, já que todas as letras maiúsculas foram excluídas.

Você também pode usar o operador `~=` em uma variável de array para excluir caracteres correspondentes de cada elemento. Por exemplo, se tivermos um array com vários nomes e quisermos excluir os sobrenomes de cada elemento, podemos fazer o seguinte:

```
set nomes João Silva José Pereira Maria Santos
set nomes = $nomes[~="* *"]
echo $nomes
```

O resultado será "João José Maria", pois o padrão "* *" indica um nome seguido de um espaço e um sobrenome, que será excluído.

## Deep Dive
O operador `~=` utiliza expressões regulares para encontrar e excluir os caracteres correspondentes. Expressões regulares são padrões de caracteres que permitem pesquisar e manipular textos de maneira flexível e poderosa. No Fish Shell, você pode encontrar mais informações sobre as expressões regulares em seu guia de ajuda digitando `help regex` no terminal.

Além disso, o Fish Shell possui outras funções relacionadas à exclusão de caracteres correspondentes, como `string match`, que retorna se uma string corresponde a um padrão e `string replace`, que substitui o padrão por outro texto. Experimente essas funções para ver como elas podem ser úteis em seus scripts!

## Veja Também
- [Guia de ajuda do Fish Shell](https://fishshell.com/docs/current/index.html)
- [Documentação sobre expressões regulares no Fish Shell](https://fishshell.com/docs/current/index.html#regex)
- [Listagem de todas as funções de manipulação de strings no Fish Shell](https://fishshell.com/docs/current/cmds/string.html)

Esperamos que este post tenha ajudado você a entender melhor a exclusão de caracteres correspondentes no Fish Shell e como aplicá-la em seu código. Explore mais funções e comandos do Fish Shell para se tornar um programador mais eficiente e produtivo!