---
title:    "Haskell: Lendo um arquivo de texto"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Porquê

Ler arquivos de texto é uma atividade comum na programação, seja para processar dados ou para ler informações de configuração. Se você é um programador Haskell, provavelmente já se deparou com a necessidade de ler um arquivo de texto em algum momento. Neste artigo, exploraremos como podemos fazer isso de forma eficiente e eficaz usando Haskell.

## Como Fazer

Para ler um arquivo de texto em Haskell, precisamos usar a função `readFile` da biblioteca padrão `System.IO`. Esta função aceita o caminho para o arquivo como argumento e retorna uma ação do tipo `IO String`, ou seja, uma ação que, quando executada, retornará uma String com o conteúdo do arquivo.

```
```Haskell
import System.IO

main = do
    arquivo <- readFile "exemplo.txt"
    putStrLn arquivo
```

Neste exemplo, estamos lendo o arquivo "exemplo.txt" e armazenando seu conteúdo na variável `arquivo`. Em seguida, usamos a função `putStrLn` para imprimir o conteúdo na tela.

Se quisermos ler apenas parte do arquivo, podemos usar a função `take` da biblioteca `Data.List`. Por exemplo, se queremos ler apenas as 10 primeiras linhas do arquivo, podemos fazer o seguinte:

```
```Haskell
import System.IO
import Data.List

main = do
    arquivo <- readFile "exemplo.txt"
    putStrLn (take 10 arquivo)
```

Além disso, se quisermos realizar algum processamento no conteúdo do arquivo, podemos usar a função `lines` da biblioteca `Data.List` para dividir o conteúdo em uma lista de linhas.

## Mergulho Profundo

Agora que sabemos como ler arquivos de texto em Haskell, vamos aprofundar um pouco mais. Ao ler um arquivo, podemos nos deparar com algumas questões, como por exemplo: e se o arquivo não existir? E se o arquivo for grande demais e causar estouro de memória? Felizmente, Haskell possui funções e tipos de dados para lidar com essas situações.

Uma vez que a função `readFile` retorna uma ação `IO`, podemos usá-la dentro de outra ação `IO` usando o operador de sequência `>>=`. Isso nos permite executar outras ações dependendo do retorno de `readFile`. Por exemplo, podemos usar a função `doesFileExist` da biblioteca `System.Directory` para verificar se o arquivo existe antes de tentar lê-lo.

```
```Haskell
import System.IO
import System.Directory

main = do
    arqExiste <- doesFileExist "exemplo.txt"
    if arqExiste
        then do
            arquivo <- readFile "exemplo.txt"
            putStrLn arquivo
        else putStrLn "Arquivo não encontrado."
```

Para lidar com o possível estouro de memória, podemos usar a função `withFile` da biblioteca `System.IO`, que abre um arquivo e o fecha automaticamente quando a ação é concluída. Além disso, podemos especificar em que modo de leitura ou escrita queremos abrir o arquivo, por exemplo, apenas para leitura ou leitura e escrita.

## Veja Também

[Documentação do módulo `System.IO`](https://hackage.haskell.org/package/base/docs/System-IO.html) \
[Documentação do módulo `Data.List`](https://hackage.haskell.org/package/base/docs/Data-List.html) \
[Documentação do módulo `System.Directory`](https://hackage.haskell.org/package/directory/docs/System-Directory.html)