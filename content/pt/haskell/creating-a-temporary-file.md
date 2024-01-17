---
title:                "Criando um arquivo temporário"
html_title:           "Haskell: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## O que e Porque?

Criar um arquivo temporario em Haskell e um processo de criar um arquivo que sera automaticamente excluido quando o programa for encerrado. Isso e util quando um programa precisa armazenar dados temporariamente e nao precisa manter o arquivo apos o termino da execucao.

## Como:

Para criar um arquivo temporario em Haskell, podemos usar a funcao `withSystemTempFile` do modulo `System.IO.Temp`. Aqui esta um exemplo de codigo:

```Haskell
import System.IO.Temp (withSystemTempFile)

main :: IO ()
main = withSystemTempFile "arquivo.txt" $ \arquivo handle -> do
  hPutStrLn handle "Este e um arquivo temporario"
  putStrLn $ "Arquivo criado em: " ++ arquivo
```

Aqui, estamos criando um arquivo chamado "arquivo.txt" e escrevendo a string "Este e um arquivo temporario" no arquivo usando o handler fornecido pela funcao `withSystemTempFile`. Apos a execucao do programa, o arquivo sera excluido automaticamente.

Se quisermos apenas criar um arquivo temporario sem escrever nada nele, podemos usar a funcao `openTempFile` do mesmo modulo. Aqui esta um exemplo de codigo:

```Haskell
import System.IO.Temp (openTempFile)

main :: IO ()
main = do
  (arquivo, handle) <- openTempFile "." "arquivo.txt"
  putStrLn $ "Arquivo criado em: " ++ arquivo
  hClose handle
```

Neste exemplo, estamos criando um arquivo temporario chamado "arquivo.txt" no diretorio atual e fechando o handler apos a criacao do arquivo.

## Profundidade

A ideia de criar arquivos temporarios existe ha bastante tempo, mas com o advento dos sistemas operacionais modernos, a exclusao automatica desses arquivos tornou-se muito mais facil e eficiente. Uma alternativa para criar arquivos temporarios em Haskell e usar o modulo `System.Posix.Temp`, que oferece uma interface diretamente com o sistema operacional POSIX.

A implementacao da funcao `withSystemTempFile` e relativamente simples, com apenas alguns passos necessarios para criar o arquivo temporario e gerenciar o seu ciclo de vida. Alem disso, o modulo `System.IO.Temp` possui varias funcoes auxiliares para trabalhar com arquivos temporarios, como copiar ou mover para outro diretorio.

## Veja tambem:

- [Documentacao do modulo `System.IO.Temp`](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-IO-Temp.html)
- [Documentacao do modulo `System.Posix.Temp`](https://hackage.haskell.org/package/unix-2.8.2.0/docs/System-Posix-Temp.html)