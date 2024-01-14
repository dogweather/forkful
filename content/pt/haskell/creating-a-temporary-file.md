---
title:                "Haskell: Criando um arquivo temporário"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por que usar arquivos temporários em Haskell?

Criar arquivos temporários é uma prática comum em linguagens de programação, incluindo Haskell. Esses arquivos são criados e usados temporariamente durante a execução de um programa e geralmente são excluídos após o uso. A criação de arquivos temporários pode ser útil para armazenar dados temporários, gerar relatórios ou até mesmo testar uma função sem alterar arquivos permanentes. Neste artigo, vamos explorar como criar e utilizar arquivos temporários em Haskell.

## Como fazer

Para criar um arquivo temporário em Haskell, primeiro precisamos importar o módulo `System.IO.Temp`. Em seguida, usamos a função `withSystemTempFile` que recebe dois parâmetros: um prefixo de nome de arquivo e uma função. Essa função é responsável por manipular o arquivo temporário criado. Abaixo, temos um exemplo de como criar um arquivo temporário com alguns dados e usá-lo em uma função:

```Haskell
import System.IO.Temp
import System.IO

myFunction :: String -> IO ()
myFunction tempFile = do
  contents <- readFile tempFile
  -- faça algo com os dados do arquivo temporário aqui
  putStrLn "Dados processados com sucesso!"

main :: IO ()
main = withSystemTempFile "temp" myFunction
```
A saída do programa será `Dados processados com sucesso!` e o arquivo temporário criado terá um nome parecido com `temp034862.txt`, dependendo do sistema operacional.

## Explorando mais a fundo

Existem outras funções relacionadas à criação de arquivos temporários em Haskell, como `withSystemTempDirectory` e `openTempFile`. A função `withSystemTempDirectory` é semelhante à `withSystemTempFile` e recebe a mesma estrutura de parâmetros, mas cria um diretório temporário em vez de um arquivo. Já a função `openTempFile` é responsável apenas pela criação do arquivo temporário, sem executar nenhuma função específica. Além disso, é possível definir o diretório em que o arquivo será criado, fornecendo-o como o terceiro parâmetro.

É importante lembrar que arquivos temporários são criados em um diretório seguro, garantindo que eles serão excluídos após o seu uso. Também é possível definir as permissões do arquivo temporário utilizando a função `openTempFileWithMode`.

## Veja também

- [Documentação do módulo `System.IO.Temp` em Haskell](https://hackage.haskell.org/package/temporary/docs/System-IO-Temp.html)
- [Tutorial sobre arquivos e diretórios temporários em Haskell](https://abhinavsarkar.net/posts/programming/2019-03-10-temporary-files-in-haskell.html)
- [Vídeo explicativo sobre arquivos temporários em Haskell](https://www.youtube.com/watch?v=HeYPJY1BrKc)