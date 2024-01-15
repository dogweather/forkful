---
title:                "Lendo um arquivo de texto."
html_title:           "Haskell: Lendo um arquivo de texto."
simple_title:         "Lendo um arquivo de texto."
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por que

Você provavelmente está lendo este artigo porque está interessado em aprender como ler arquivos de texto em Haskell. Ler e processar arquivos de texto é uma tarefa comum em muitos programas, e entender como fazer isso em Haskell pode expandir suas habilidades de programação.

## Como Fazer

Ler um arquivo de texto em Haskell é uma tarefa relativamente simples. Primeiro, precisamos importar o módulo System.IO, que nos dará acesso às funções de E/S (entrada/saída) de arquivos. Em seguida, usaremos a função `readFile`, que aceita o caminho para o arquivo como seu argumento e retorna o conteúdo do arquivo em formato de string.

```
import System.IO

main = do
    conteudo <- readFile "arquivo.txt"
    print conteudo
```

Isso irá imprimir o conteúdo do arquivo na tela. Note que a função `readFile` retorna um valor do tipo `IO String`, mas usamos o operador `<-` para extrair o valor de dentro do `IO` e atribuí-lo à variável `conteudo`.

Se quisermos ler apenas uma linha do arquivo, podemos usar a função `readLine`, que funciona de forma semelhante à `readFile`. Por exemplo:

```
import System.IO

main = do
    linha <- readLine "arquivo.txt"
    print linha
```

Agora, se quisermos ler o arquivo linha por linha e imprimir as linhas na tela, podemos usar a função `readLines`, que retorna uma lista contendo todas as linhas do arquivo. Podemos então usar a função `mapM_` para imprimir cada linha individualmente.

```
import System.IO

main = do
    linhas <- readLines "arquivo.txt"
    mapM_ print linhas
```

## Mergulho Profundo

Além das funções `readFile`, `readLine` e `readLines`, o módulo System.IO também possui outras funções úteis para trabalhar com arquivos de texto. Aqui estão algumas delas:

- `writeFile` - aceita o caminho para o arquivo e uma string contendo o conteúdo que queremos escrever no arquivo, e escreve essa string no arquivo.
- `appendFile` - semelhante à `writeFile`, mas adiciona o conteúdo fornecido ao final do arquivo, em vez de substituir o conteúdo existente.
- `openFile` - aceita o caminho para o arquivo e um modo de abertura (pode ser leitura, escrita, anexar, etc.), e retorna uma handle de arquivo que pode ser usada com funções como `hGetLine` e `hPutStrLn`.
- `withFile` - uma versão mais segura de `openFile`, que garante que o arquivo será fechado corretamente após seu uso.

A documentação do módulo System.IO possui mais informações sobre essas e outras funções.

## Veja Também

- [Documentação do módulo System.IO](https://hackage.haskell.org/package/base/docs/System-IO.html)
- [Tutorial de arquivo de texto em Haskell](https://learn.hfm.io/files.html)
- [Guia rápido de Haskell](https://tryhaskell.org/)