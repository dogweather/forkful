---
title:                "Escrevendo um arquivo de texto"
html_title:           "Fish Shell: Escrevendo um arquivo de texto"
simple_title:         "Escrevendo um arquivo de texto"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## O que & Porquê?
 Escrever um arquivo de texto é simplesmente criar um documento que contém palavras e informações que podem ser lidas e interpretadas por um computador. Programadores frequentemente escrevem arquivos de texto como parte do processo de codificação de um programa ou script.

## Como fazer:
Existem várias maneiras de escrever um arquivo de texto no Fish Shell. Aqui estão três exemplos da sintaxe básica e seu output correspondente:

```
# Exemplo 1: Escrevendo um único linha no arquivo de texto "meu_arquivo.txt"
> set conteudo "Este é um exemplo de texto."
> echo $conteudo > meu_arquivo.txt

# Output:
> cat meu_arquivo.txt
Este é um exemplo de texto.
```

```
# Exemplo 2: Escrevendo múltiplas linhas no arquivo de texto "meu_arquivo.txt"
> set linhas "Primeira linha\nSegunda linha\nTerceira linha"
> echo -e $linhas > meu_arquivo.txt

# Output:
> cat meu_arquivo.txt
Primeira linha
Segunda linha
Terceira linha
```

```
# Exemplo 3: Acrescentando conteúdo em um arquivo de texto existente
> set novo_conteudo "Quarta linha"
> echo $novo_conteudo >> meu_arquivo.txt

# Output:
> cat meu_arquivo.txt
Primeira linha
Segunda linha
Terceira linha
Quarta linha
```

## Profundidade:
Escrever arquivos de texto é uma prática comum entre programadores desde os primórdios da programação. Alternativas ao Fish Shell incluem Bash, Zsh e outras shells de linha de comando. O comando "echo" é usado para escrever conteúdo em um arquivo de texto, enquanto o sinal ">" é usado para criar um novo arquivo ou substituir o conteúdo de um arquivo existente e ">>" é usado para acrescentar conteúdo em um arquivo existente.

## Ver também:
- [Documentação oficial do Fish Shell sobre o comando "echo"](https://fishshell.com/docs/current/commands.html#echo)
- [Tutorial do Linux sobre como escrever em arquivos de texto](https://linuxconfig.org/how-to-append-to-a-text-file-using-fish-shell-in-linux)
- [Exemplos de uso do comando "echo" no Fish Shell](https://www.cyberciti.biz/faq/bash-append-text-to-a-file/)