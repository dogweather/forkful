---
title:                "Haskell: Lendo um arquivo de texto"
simple_title:         "Lendo um arquivo de texto"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por que ler um arquivo de texto em Haskell?

Ler arquivos de texto é uma tarefa comum e útil na programação. Em Haskell, isso pode ser feito de forma eficiente e elegante, e entender como isso funciona é fundamental para qualquer programador da linguagem. Neste artigo, vamos mergulhar no processo de leitura de arquivos de texto em Haskell e explorar como esta poderosa funcionalidade pode ser usada em seus projetos.

## Como fazer isso?

Para ler um arquivo de texto em Haskell, precisamos primeiro importar o módulo `System.IO` e utilizar a função `readFile` que está presente nele. Esta função recebe um caminho para o arquivo de texto e retorna uma ação do tipo `IO String`, que contém todo o conteúdo do arquivo como uma única string.

```Haskell
import System.IO

main = do
  file <- readFile "arquivo.txt"
  putStrLn file
```

No exemplo acima, estamos lendo o conteúdo do arquivo "arquivo.txt" e imprimindo-o na tela utilizando a função `putStrLn`. Note que é importante utilizar a função `main` e a monada `do` para executarmos uma ação do tipo `IO`. Além disso, é necessário tratar possíveis erros ao tentar ler o arquivo, o que pode ser feito utilizando a função `catch` do módulo `Control.Exception`.

```Haskell
import System.IO
import Control.Exception

main = do
  file <- catch (readFile "arquivo.txt") (return . show)
  putStrLn file
```

No exemplo acima, caso ocorra um erro ao tentar ler o arquivo, a função `catch` retornará uma string com a descrição do erro, que será impressa na tela.

## Profundidade do processo de leitura

Em Haskell, a leitura de um arquivo de texto é um processo simples, mas é importante entender como isso funciona nos bastidores. Quando utilizamos a função `readFile`, Haskell não lê todo o conteúdo do arquivo de uma vez. Na verdade, ele cria uma "promessa" de ler o arquivo no futuro, utilizando a monada `IO`. Isso significa que podemos realizar outras ações antes de realmente ler o arquivo, tornando o processo mais eficiente. Além disso, a função `readFile` cuida automaticamente do fechamento do arquivo após a leitura, evitando vazamentos de memória.

## Veja também

Se você quer se aprofundar ainda mais no assunto, confira os links abaixo:

- [Documentação oficial do módulo System.IO](https://hackage.haskell.org/package/base-4.12.0.0/docs/System-IO.html)
- [Tutorial da Wikibooks sobre leitura de arquivos em Haskell](https://en.wikibooks.org/wiki/Haskell/Input_and_output#Reading_files)

Espero que este artigo tenha sido útil e que você possa aplicar esses conhecimentos em seus projetos futuros. Happy coding!