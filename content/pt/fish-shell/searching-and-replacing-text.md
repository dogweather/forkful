---
title:    "Fish Shell: Busca e substituição de texto"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que usar a linguagem Fish Shell?

A linguagem Fish Shell é um interpretador de comandos que oferece uma abordagem mais fácil e intuitiva para a substituição de texto. Ao invés de precisar compreender expressões regulares, o Fish Shell permite que você procure e altere rapidamente o texto desejado de forma simples.

## Como usar a substituição de texto no Fish Shell

Para utilizar a substituição de texto no Fish Shell, você pode seguir os seguintes passos:

1. Abra o Fish Shell no seu terminal
2. Digite o comando `sed "s/ANTIGO/NOVO/g" arquivo` onde "ANTIGO" é o texto que você deseja substituir e "NOVO" é o texto que será inserido no lugar
3. Pressione Enter e observe o resultado do comando

Por exemplo, se você quisesse substituir todas as ocorrências da palavra "amarelo" por "azul" em um arquivo de texto chamado "cores.txt", o comando seria `sed "s/amarelo/azul/g" cores.txt`.

Isso irá substituir todas as palavras "amarelo" por "azul" no arquivo "cores.txt", sem a necessidade de usar expressões regulares complicadas.

```
Fish Shell
~/Desktop > sed "s/amarelo/azul/g" cores.txt
vermelho
azul
verde
preto
azul
```

## Mais sobre a substituição de texto no Fish Shell

O comando `sed` utilizado no Fish Shell é baseado no utilitário de linha de comando sed, que é um poderoso editor de texto em UNIX. No Fish Shell, ele permite que você substitua o texto desejado em um arquivo sem a necessidade de utilizar uma sintaxe complexa de expressões regulares.

Além disso, é possível adicionar opções ao comando `sed`, como por exemplo, especificar qual linha do arquivo deve ser alterada ou utilizar caracteres especiais para delimitar o texto a ser substituído.

Para mais informações sobre o comando `sed` no Fish Shell, você pode consultar a documentação oficial [aqui](https://fishshell.com/docs/current/cmds/sed.html).

## Veja também

- [Documentação oficial do Fish Shell](https://fishshell.com/docs/current/)
- [Guia rápido de comandos do Fish Shell](https://fishshell.com/docs/current/cmds.html)
- [Tutorial interativo de Fish Shell](https://fishshell.com/docs/current/tutorial.html)