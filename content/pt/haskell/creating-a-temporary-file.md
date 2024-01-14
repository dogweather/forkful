---
title:    "Haskell: Criando um arquivo temporário"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Porque

Criar arquivos temporários pode ser muito útil em programação quando se trabalha com dados que não precisam ser armazenados permanentemente. Por exemplo, se você está escrevendo um programa para processar dados de um CSV, pode ser útil criar um arquivo temporário para armazenar esses dados durante o processamento.

## Como Fazer

A linguagem de programação Haskell tem uma maneira simples e elegante de criar arquivos temporários. Aqui está um exemplo de como fazer isso:

```Haskell
import System.IO
import System.Directory

-- Cria um arquivo temporário chamado "tempFile.txt"
createTempFile = do
    tempDir <- getTemporaryDirectory 
    (tempFilePath, tempHandle) <- openTempFile tempDir "tempFile.txt"
    putStrLn $ "O caminho do arquivo temporário é: " ++ tempFilePath
    hClose tempHandle
```

Este código primeiro importa as funções `getTemporaryDirectory` e `openTempFile` do módulo `System.IO`. Então, a função `createTempFile` é definida, que primeiro obtém o diretório temporário atual usando `getTemporaryDirectory`. Em seguida, `openTempFile` é usado para criar e abrir um arquivo temporário com o nome "tempFile.txt" no diretório temporário. O caminho do arquivo temporário é impresso na tela e, por fim, o arquivo é fechado.

Se você executar este código, verá que um arquivo chamado "tempFile.txt" foi criado no diretório temporário especificado e o caminho desse arquivo será impresso na tela.

## Profundidade

Se você quiser saber mais sobre como o Haskell lida com arquivos temporários, pode ser útil entender como a função `getTemporaryDirectory` é implementada. Aqui está uma versão simplificada da implementação dessa função:

```Haskell
getTemporaryDirectory :: IO String
getTemporaryDirectory = MaybeT (System.Environment.lookupEnv "TEMP") 
    >>= \tempDir -> if isJust tempDir then return (fromJust tempDir) 
        else MaybeT (System.Environment.lookupEnv "TMP") 
            >>= \tmpDir -> if isJust tmpDir then return (fromJust tmpDir) 
                else fail "Não foi possível encontrar um diretório temporário."
```

Esta implementação usa monads para lidar com possíveis falhas. Primeiro, tenta obter o diretório temporário usando a variável de ambiente "TEMP". Se isso falhar, ele tentará obter usando a variável de ambiente "TMP". Se ambas as tentativas falharem, uma mensagem de erro será emitida.

## Veja Também

- [Documentação oficial do Haskell](https://www.haskell.org/documentation/)
- [Tutorial de Haskell](https://wiki.haskell.org/Tutorial)
- [Guia de sintaxe do Haskell](https://www.tutorialspoint.com/haskell/haskell_syntax.htm)