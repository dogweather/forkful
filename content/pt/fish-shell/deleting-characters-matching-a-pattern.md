---
title:                "Fish Shell: Excluindo caracteres correspondentes a um padrão"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Porquê
Deletar caracteres que correspondam a um padrão pode ser extremamente útil para limpar dados, desenvolver scripts automatizados ou simplificar tarefas no Fish Shell. Esta funcionalidade permite que os programadores excluam rapidamente partes específicas de um texto ou código, economizando tempo e esforço.

## Como Fazer
Felizmente, a linguagem Fish Shell é ótima para manipular strings e oferece uma variedade de opções para excluir caracteres que correspondam a um padrão. Aqui está um exemplo de código mostrando como fazer isso:

```Fish Shell
set variavel = "Olá, Mundo!"
echo $variavel
Olá, Mundo!
set novo_variavel = $variavel global-substituir -r "M" ""
echo $novo_variavel
Olá, undo!
```

Neste exemplo, usamos a função `global-substituir` com a opção `-r` para substituir todas as ocorrências da letra "M" na variável original (`$variavel`) por um espaço em branco. Isso é feito ao definir uma nova variável com o mesmo valor da primeira, seguida pelo comando `global-substituir` que exclui o padrão especificado. O resultado é uma nova variável (`$novo_variavel`) com a letra "M" removida da string original.

Você também pode usar outras opções, como `local-substituir` que irá substituir apenas a primeira ocorrência do padrão, ou `substring-substituir`, que irá excluir uma seção específica da string. Experimente diferentes opções e veja qual funciona melhor para as suas necessidades!

## Explorando mais a fundo
Se você é novo no uso da linguagem Fish Shell ou está procurando mais informações sobre como excluir caracteres correspondentes a um padrão, aqui está uma lista de recursos úteis para ajudá-lo a se aprofundar:

- [Documentação oficial do Fish Shell](https://fishshell.com/docs/current/cmds/global-substitute.html)
- [Fórum de discussão do Fish Shell](https://fishshell.com/docs/current/cmds/global-substitute.html)
- [Vídeos tutoriais sobre o Fish Shell](https://www.youtube.com/watch?v=fEIIPJQNvCI)
- [Comunidade Fish Shell no Reddit](https://www.reddit.com/r/fishshell/)

## Veja também
Confira estes recursos adicionais para aprender mais sobre a linguagem Fish Shell e suas funcionalidades incríveis:

- [Shell Scripting com Fish Shell: Um guia abrangente](https://raymondxie.medium.com/shell-scripting-with-fish-shell-a-comprehensive-guide-c44ac6695853)
- [Dominando a linguagem Fish Shell](https://dev.to/franchessinee/mastering-the-fish-shell-5cano)