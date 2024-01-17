---
title:                "Lendo um arquivo de texto"
html_title:           "Bash: Lendo um arquivo de texto"
simple_title:         "Lendo um arquivo de texto"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

## O que e por que?

Ler um arquivo de texto é simplesmente o ato de visualizar o conteúdo de um arquivo de texto em um editor de texto ou terminal. Os programadores geralmente leem arquivos de texto para acessar e manipular dados.

## Como fazer:

Um exemplo simples de ler um arquivo de texto em Bash é usando o comando `cat`. Por exemplo, se você tiver um arquivo de texto chamado `texto.txt` com o seguinte conteúdo:

```Bash
Olá mundo!
Este é um texto de exemplo.
```

Você pode usar o comando `cat texto.txt` para exibir o conteúdo do arquivo no seu terminal:

```Bash
$ cat texto.txt

Olá mundo!
Este é um texto de exemplo.
```

Outro comando útil para ler arquivos de texto é `head`, que exibe as primeiras linhas do arquivo. Por exemplo, se você quiser ver as duas primeiras linhas do arquivo `texto.txt`, você pode usar `head -n 2 texto.txt`:

```Bash
$ head -n 2 texto.txt

Olá mundo!
Este é um texto de exemplo.
```

## Profundidade

Ler arquivos de texto é uma tarefa básica na programação, e é usada para obter dados a serem manipulados em um script ou programa. No passado, o comando `cat` era usado principalmente para visualizar o conteúdo de arquivos de texto, mas hoje em dia existem outras opções, como `head`, `tail` e `less`.

Além disso, os desenvolvedores também podem usar recursos mais avançados, como expressões regulares e loops, para ler arquivos de texto de forma mais precisa e eficiente. Esses recursos podem ser particularmente úteis ao lidar com grandes conjuntos de dados.

## Veja também:

Se você quiser aprender mais sobre como ler arquivos de texto em Bash, consulte a documentação oficial do Bash ou confira estes recursos adicionais:

- [Tutorial sobre leitura de arquivos de texto em Bash](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)
- [Guia de expressões regulares em Bash](https://www.regular-expressions.info/bash.html)
- [Aprenda Bash em 15 minutos](https://www.panix.com/~elflord/unix/bash-tute.html#reads)