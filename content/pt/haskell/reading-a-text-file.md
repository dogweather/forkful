---
title:    "Haskell: Lendo um arquivo de texto"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Por que

Se você é um programador Haskell, provavelmente já ouviu falar sobre a capacidade dessa linguagem de lidar com arquivos de texto. Mas por que você deveria se preocupar em ler arquivos de texto em seu código? Bem, a leitura de arquivos de texto pode ser útil em diversas aplicações, como processamento de dados, criação de relatórios e muito mais. Além disso, é uma habilidade importante para qualquer programador estar familiarizado.

## Como Fazer

Para ler um arquivo de texto em Haskell, primeiro precisamos importar o módulo `System.IO`. Em seguida, podemos usar a função `readFile`, que recebe o caminho do arquivo como parâmetro e retorna uma string contendo o conteúdo do arquivo. Vamos ver um exemplo de código:

```Haskell
import System.IO

main = do
    contents <- readFile "arquivo.txt"
    putStrLn contents
```

Neste exemplo, estamos lendo o conteúdo do arquivo "arquivo.txt" e imprimindo-o no console. Podemos usar qualquer função que operar em strings para processar esse conteúdo e utilizá-lo em nossos programas.

## Mergulho Profundo

Além da função `readFile`, o módulo `System.IO` possui outras funções úteis para trabalhar com arquivos de texto, como `openFile`, que permite abrir um arquivo em diferentes modos (leitura, escrita, entre outros), e `hGetContents`, que pode ser usado para ler conteúdo de um arquivo linha por linha. Além disso, o tipo de dados `FilePath` é usado para representar caminhos de arquivos de forma segura e portável em diferentes sistemas operacionais.

É importante lembrar também de sempre fechar um arquivo após sua leitura ou escrita, usando a função `hClose`, para evitar vazamentos de memória.

## Veja Também

- [Documentação sobre o módulo System.IO](https://hackage.haskell.org/package/base-4.12.0.0/docs/System-IO.html)
- [Tutorial sobre leitura de arquivos em Haskell](https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/reading-and-writing-files)
- [Guia completo de Haskell](http://learnyouahaskell.com/)