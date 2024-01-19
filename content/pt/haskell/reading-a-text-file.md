---
title:                "Lendo um arquivo de texto"
html_title:           "Bash: Lendo um arquivo de texto"
simple_title:         "Lendo um arquivo de texto"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Ler um arquivo de texto envolve interpretar e manipular o conteúdo salvo em um arquivo de texto. Os programadores fazem isso para recuperar e processar dados armazenados em arquivos externos.

## Como Fazer:

Aqui está um exemplo simples de como ler um arquivo de texto em Haskell:

```Haskell
main :: IO ()
main = do
    conteudo <- readFile "meuArquivo.txt"
    putStrLn conteudo
```
Quando rodar esse programa, você verá o conteúdo de `meuArquivo.txt` na tela.

Vamos tentar um mais complexo que conta o número de linhas:

```Haskell
main :: IO ()
main = do
    conteudo <- readFile "meuArquivo.txt"
    print $ length $ lines conteudo
```
Isso imprimirá o número de linhas no arquivo `meuArquivo.txt`.

## Mergulho Profundo:

Este tipo de operação de leitura de arquivo tem sido uma necessidade comum na programação desde os primórdios. Em Haskell, a função `readFile` é freqüentemente usada para esse propósito. Ela faz parte de `System.IO`, um dos módulos predefinidos.

Existem várias alternativas para ler um arquivo de texto em Haskell. Algumas bibliotecas, como `Data.ByteString` e `Data.Text`, oferecem funções de leitura de arquivos que podem ser mais eficientes para grandes arquivos ou para manipulação de strings mais avançada.

Quando você usa `readFile`, o Haskell lida automaticamente com muitos detalhes de implementação para você. Ele abre o arquivo, lê o conteúdo para a memória como uma string e, em seguida, fecha o arquivo. O conteúdo é lido de uma vez, por isso pode não ser adequado para arquivos muito grandes.

## Veja Também:

- Módulo System.IO em Hackage: (<http://hackage.haskell.org/package/base-4.14.1.0/docs/System-IO.html#v:readFile>)
- Tópicos relacionados em Learn You a Haskell: (<http://learnyouahaskell.com/input-and-output>)
- Alternativas de leitura de arquivo em Stackage: Data.Text.IO (<https://www.stackage.org/haddock/lts-17.4/text-1.2.4.1/Data-Text-IO.html>) e Data.ByteString (<https://hackage.haskell.org/package/bytestring-0.11.1.0/docs/Data-ByteString.html>)