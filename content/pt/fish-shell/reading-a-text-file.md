---
title:                "Lendo um arquivo de texto"
html_title:           "Bash: Lendo um arquivo de texto"
simple_title:         "Lendo um arquivo de texto"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/fish-shell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## O Que & Por quê? 

Ler um arquivo de texto refere-se ao processo de acessar e extrair os dados armazenados em um arquivo de texto (.txt) usando um programa de computador. Programadores fazem isso para manipular ou explorar esses dados para satisfazer uma gama de requisitos de aplicativos.

## Como fazer:

No Fish Shell, podemos usar o comando `cat` para ler o conteúdo de um arquivo de texto. Aqui está um exemplo de como você pode ler um arquivo de texto chamado 'exemplo.txt'.

```Fish Shell
cat exemplo.txt
```
Suponha que 'exemplo.txt' contenha o seguinte

```Fish Shell
Isto é um exemplo
Bem-vindo ao Fish Shell
```
Correndo o comando `cat exemplo.txt` exibirá o conteúdo acima no console.

## Deep Dive

Historicamente, a leitura de arquivos de texto é uma das operações mais antigas realizadas em sistemas de computador. `cat` é uma dessas ferramentas pioneiras que vem do Unix original.

Existem alternativas ao comando `cat` se você deseja mais flexibilidade ou detalhes do arquivo. Por exemplo, `less` permite rolar um arquivo e `head` ou `tail` mostram o início ou o fim de um arquivo, respectivamente.

Embora a implementação detalhada possa variar, a leitura de um arquivo de texto geralmente envolve a abertura do arquivo em um modo de leitura, a leitura do conteúdo do arquivo em uma ou mais strings e, então, a manipulação ou exibição dessas strings.

## Veja Também

Se você está aprendendo a usar o Fish Shell, as seguintes fontes relacionadas podem ser úteis:

1. [Documentação oficial do Fish](https://fishshell.com/docs/current/index.html)
2. [Tutorial de script Fish](https://fishshell.com/docs/current/tutorial.html)