---
title:                "Criando um arquivo temporário"
html_title:           "Bash: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Criar um arquivo temporário é uma tarefa que envolve a criação de um arquivo que serve apenas durante a duração de uma determinada sessão ou processo. Os programadores costumam fazer isso quando estão trabalhando com grandes volumes de dados ou quando precisam de um espaço para armazenar dados intermediários.

## Como Fazer:

Vamos fazer uso da biblioteca `System.IO.Temp` do Haskell. Segue abaixo um exemplo de como você pode criar um arquivo temporário. 

```Haskell
import System.IO.Temp (withSystemTempFile)
import System.IO (hPutStrLn, hClose)

main = do
    withSystemTempFile "temp.txt" $ \tempFilePath hFile -> do
        hPutStrLn hFile "Alguma data aqui."
        hClose hFile
```
Executando este código, Haskell criará um arquivo temporário chamado `temp.txt`, escreverá "Alguma data aqui." nele e então fechará o arquivo.

## Mergulho Profundo

Historicamente, a criação de arquivos temporários tem sido uma prática comum na programação, especialmente em situações em que o volume de dados ou a complexidade dos cálculos tornam impraticável manter todos os dados na memória. 

Uma alternativa a criação de arquivos temporários seria usar estruturas de dados na memória, como listas ou arrays. No entanto, isso pode ser menos eficiente se os dados forem massivos e nem sempre é prático.

A função `withSystemTempFile` em Haskell cria um novo arquivo temporário no sistema de arquivos, escreve no arquivo usando a função de manipulação fornecida e, em seguida, fecha o arquivo. Importante ressaltar que `withSystemTempFile` abrirá e fechará o arquivo para você e garantirá que o arquivo seja excluído quando não for mais necessário.

## Ver Também

Segue abaixo alguns links úteis para consulta posterior.

- Documentação da System.IO.Temp: [aqui](http://hackage.haskell.org/package/temporary-1.3/docs/System-IO-Temp.html)
- Tutorial Haskell para trabalhar com arquivos: [aqui](https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/files-and-directories)