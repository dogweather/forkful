---
title:                "Bash: Lendo um arquivo de texto"
programming_language: "Bash"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por que

Se você está aprendendo a programar em Bash, pode se perguntar por que seria importante saber ler arquivos de texto. A resposta é simples - a leitura de arquivos de texto é uma habilidade fundamental para automatizar processos e manipular grandes quantidades de dados em um sistema operacional Linux. Se você quiser aprender a trabalhar com Bash, entender como ler arquivos de texto é fundamental.

## Como Fazer

Para ler um arquivo de texto em Bash, primeiramente você precisa criar um arquivo de texto para ler. Você pode usar qualquer editor de texto, como o Nano ou o VI, para criar um arquivo simples com algumas linhas de texto. Depois de salvar o arquivo, você pode usar o comando "cat" para exibir o conteúdo do arquivo no terminal.

Para ler o arquivo em um script Bash, você pode usar o comando "read" seguido do nome do arquivo. O exemplo abaixo mostra como ler o conteúdo de um arquivo chamado "texto.txt" e atribuí-lo a uma variável chamada "texto".

```Bash
read texto < texto.txt
echo $texto
```

Se você quiser ler linha por linha do arquivo, pode usar um loop while com o comando "read", como mostrado no exemplo abaixo:

```Bash
while read linha; do
    echo $linha
done < texto.txt
```

O comando "read" é uma maneira útil de permitir que seu script Bash interaja com a entrada do usuário e arquivos de texto.

## Deep Dive

Quando você lê um arquivo de texto em Bash, o sistema basicamente armazena o conteúdo do arquivo em uma variável. Isso significa que você pode manipulá-lo da mesma maneira que faria com qualquer outra variável em Bash. Por exemplo, você pode concatenar várias variáveis e redirecionar o resultado para um novo arquivo de texto.

Além disso, você também pode usar o comando "grep" para pesquisar por padrões específicos no conteúdo do arquivo. Isso é especialmente útil quando você precisa analisar grandes arquivos de texto em busca de informações importantes.

## Veja também

- [Tutorial de Script em Bash](https://www.tecmint.com/writing-shell-scripts/)
- [Referência Bash para Leitura de Arquivos de Texto](https://linuxhint.com/bash_read_file_grep_input/)
- [Cursos de programação Bash](https://www.udemy.com/topic/bash-scripting/)