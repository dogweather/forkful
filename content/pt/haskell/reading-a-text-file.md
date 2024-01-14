---
title:                "Haskell: Lendo um arquivo de texto"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por que ler um arquivo de texto em Haskell?

Ler um arquivo de texto pode ser uma tarefa comum em muitos projetos de programação, independentemente da linguagem escolhida. No entanto, em Haskell, a leitura de arquivos de texto pode se tornar uma tarefa simples e elegante devido à sua sintaxe funcional e bibliotecas poderosas. Neste artigo, iremos explorar por que vale a pena aprender a ler arquivos de texto em Haskell.

## Como ler um arquivo de texto em Haskell

Para ler um arquivo de texto em Haskell, precisamos primeiro abrir o arquivo e armazená-lo em uma variável. Podemos fazer isso usando a função `openFile` da biblioteca `System.IO`. Esta função possui três parâmetros: o nome do arquivo, o modo de abertura (leitura, escrita, etc.) e um identificador de encoding, que define como o arquivo será interpretado (UTF-8, Latin-1, etc.).

````Haskell
import System.IO

main = do
  handle <- openFile "exemplo.txt" ReadMode
  contents <- hGetContents handle
  putStrLn contents
  hClose handle
````
Neste exemplo, o arquivo "exemplo.txt" é aberto com o modo de leitura e o conteúdo do arquivo é armazenado na variável `contents`. Usamos então a função `putStrLn` para imprimir o conteúdo na tela e `hClose` para fechar o arquivo.

## Aprofundando na leitura de arquivos de texto em Haskell

Além da função `openFile`, existem outras funções úteis para lidar com arquivos de texto em Haskell. Por exemplo, a função `readFile` que abre, lê e fecha o arquivo automaticamente, e a função `writeFile` que escreve no arquivo.

Devemos lembrar que ao ler um arquivo, o conteúdo é retornado na forma de uma string. Portanto, se quisermos trabalhar com esses dados, podemos usar funções de manipulação de strings disponíveis em Haskell, como `lines` para separar o texto em uma lista de linhas ou `words` para obter uma lista de palavras.

Além disso, vale ressaltar que a funcão `openFile` pode levantar uma exceção caso o arquivo não seja encontrado ou não tenha permissão de leitura. Por isso, é importante lidar com possíveis erros ao lidar com arquivos de texto em Haskell.

## Veja também

- [Documentação sobre leitura e escrita de arquivos em Haskell](https://www.haskell.org/onlinereport/io.html)
- [Tutoriais de Haskell da Wikibooks](https://en.wikibooks.org/wiki/Haskell)
- [Artigo sobre manipulação de strings em Haskell](https://wiki.haskell.org/Strings)