---
title:                "Fish Shell: Lendo um arquivo de texto"
programming_language: "Fish Shell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por que

Muitas vezes, ao trabalhar com programação, precisamos manipular arquivos de texto. Seja para extrair informações específicas ou para fazer mudanças em massa, a leitura de arquivos de texto é uma tarefa comum em muitos projetos. Neste artigo, vamos mostrar como é fácil e útil ler um arquivo de texto usando o Fish Shell.

## Como fazer

Para ler um arquivo de texto usando o Fish Shell, primeiro precisamos usar o comando `cat` juntamente com o nome do arquivo. Por exemplo, se quisermos ler um arquivo chamado "dados.txt" que está localizado em nosso diretório atual, podemos usar o seguinte comando:

```Fish Shell
cat dados.txt
```

Isso irá imprimir todo o conteúdo do arquivo no terminal. Mas e se quisermos apenas as primeiras linhas do arquivo? Podemos usar o comando `head` com o nome do arquivo e a opção `-n`, especificando o número de linhas que queremos imprimir. Por exemplo, se quisermos imprimir apenas as primeiras 10 linhas do arquivo, podemos usar o comando:

```Fish Shell
head -n 10 dados.txt
```

Da mesma forma, se quisermos imprimir apenas as últimas linhas do arquivo, podemos usar o comando `tail` combinado com a opção `-n`. Por exemplo, para imprimir as últimas 5 linhas do arquivo, podemos usar o comando:

```Fish Shell
tail -n 5 dados.txt
```

## Mergulho profundo

Existem muitas outras opções e comandos que podemos usar para ler e manipular arquivos de texto no Fish Shell. Por exemplo, podemos usar o comando `grep` para procurar por padrões específicos no arquivo, ou o comando `sed` para fazer alterações em linhas específicas. Além disso, o Fish Shell oferece suporte a expressões regulares, o que pode ser muito útil ao lidar com arquivos de texto.

## Veja também

- [Documentação oficial do Fish Shell](https://fishshell.com/docs/current/)
- [Tutorial de linha de comando do Fish Shell](https://techexpert.tips/pt-br/fish-shell-pt-br/como-usar-comandos-de-linha-comando-fish-shell-linux/)