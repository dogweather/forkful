---
title:                "Bash: Lendo um arquivo de texto"
simple_title:         "Lendo um arquivo de texto"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Por que ler um arquivo de texto no Bash?

Ler arquivos de texto é uma tarefa comum em programação e pode ser feito de várias maneiras. Neste artigo, vamos explorar como ler um arquivo de texto no Bash e discutir algumas razões pelas quais você pode querer fazer isso.

## Como ler um arquivo de texto no Bash

Para ler um arquivo de texto no Bash, usaremos o comando `cat`. Este comando é usado para imprimir o conteúdo de um arquivo e pode ser usado para ler um arquivo de texto.

```
$ cat texto.txt
```

O comando `cat` imprimirá o conteúdo do arquivo `texto.txt` no terminal. Você também pode usar o redirecionamento `>` para enviar o conteúdo do arquivo para outro local, como um novo arquivo ou para a entrada de outro comando.

```
$ cat texto.txt > novo_texto.txt
```

Você também pode usar o `cat` junto com outros comandos para executar ações diferentes no texto, como filtrá-lo usando o comando `grep` ou selecionar linhas específicas com o comando `sed`.

## Mergulhando mais fundo

Além do comando `cat`, existem outros comandos que podem ser usados para realizar a leitura de um arquivo de texto no Bash. Por exemplo, o comando `head` permite visualizar as primeiras linhas do arquivo, enquanto o comando `tail` mostra as últimas linhas.

```
$ head texto.txt
$ tail texto.txt
```

Também é possível usar o comando `less` para visualizar o conteúdo do arquivo de forma paginada, o que pode ser útil para arquivos de texto longos.

```
$ less texto.txt
```

Além disso, você pode usar o operador de redirecionamento `<<` para permitir que o usuário forneça entradas para um script do Bash a partir de um arquivo de texto.

```
$ ./script.sh << texto.txt
```

Aprender a ler arquivos de texto no Bash pode ser útil em muitas situações, como manipulação de dados, criação de scripts e automatização de tarefas.

## Veja também

- [Tutorial de introdução ao Bash](https://www.digitalocean.com/community/tutorials/basics-of-shell-scripting)
- [Comandos de redirecionamento no Bash](https://www.tecmint.com/input-output-redirection-operators-in-linux/)
- [Documentação oficial do Bash](https://www.gnu.org/software/bash/manual/html_node/)