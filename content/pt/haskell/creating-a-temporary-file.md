---
title:                "Haskell: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por que criar um arquivo temporário em Haskell?

Ao trabalhar com programação, muitas vezes nos deparamos com a necessidade de utilizar arquivos temporários para armazenar dados de forma temporária durante a execução de nosso código. Em Haskell não é diferente, e a criação de arquivos temporários pode ser muito útil em diferentes situações. Neste artigo, vamos explorar como criar e utilizar arquivos temporários em Haskell.

## Como criar e utilizar um arquivo temporário em Haskell

Para criar um arquivo temporário em Haskell, precisamos primeiro importar o módulo "System.IO" em nosso código. Em seguida, utilizamos a função "withSystemTempFile" que recebe como parâmetros uma função que irá utilizar o arquivo temporário criado, e uma string com um nome de arquivo base. Por exemplo:

```Haskell
import System.IO 

main = do 
  withSystemTempFile "temp" $ \tempFilePath tempHandle -> do
    hPutStrLn tempHandle "Este é um arquivo temporário" 
```

Neste código, a função "withSystemTempFile" irá criar um arquivo temporário com o nome "temp" e executar a função dentro do bloco "do" utilizando o caminho e o handle (ou ponteiro) do arquivo temporário criado. Dentro do bloco, podemos utilizar funções de manipulação de arquivos, como a "hPutStrLn" que irá escrever a string no arquivo temporário. Ao final do bloco, o arquivo temporário é automaticamente fechado e deletado.

Além disso, podemos utilizar outras funções como "withTempDirectory" para criar diretórios temporários ou "withTempFile" para criar apenas um arquivo temporário sem a necessidade do handle. Porém, a função "withSystemTempFile" é geralmente a mais utilizada e recomendada.

## Mais informações sobre a criação de arquivos temporários em Haskell

Ao criar um arquivo temporário em Haskell, o sistema operacional é responsável por definir a localização e nome do arquivo, garantindo que seja único e seguro para o uso em nosso código. Além disso, o arquivo temporário é armazenado na pasta padrão do sistema para arquivos temporários.

Caso seja necessário acessar novamente o arquivo temporário após a sua criação, podemos utilizar a função "withFile" passando o caminho do arquivo, o modo de abertura e a função a ser executada.

## Veja também

- [Documentação oficial sobre a criação de arquivos temporários em Haskell](https://hackage.haskell.org/package/temporary-1.3/docs/System-IO-Temp.html)
- [Exemplo de uso da função "withSystemTempFile"](https://riptutorial.com/pt/haskell/example/6634/como-criar-um-arquivo-temporario)
- [Explicação detalhada sobre a criação de arquivos temporários em Haskell](https://www.fpcomplete.com/blog/2016/01/concise-io-via-temporary-files/)