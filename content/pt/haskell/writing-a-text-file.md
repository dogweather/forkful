---
title:                "Haskell: Escrevendo um arquivo de texto"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por que

Escrever um arquivo de texto é uma tarefa comum na programação, principalmente em Haskell. Isso porque os arquivos de texto são uma forma simples e eficiente de armazenar e manipular dados em programas.

## Como Fazer

Para escrever um arquivo de texto em Haskell, primeiro precisamos importar o módulo `System.IO`, que nos fornecerá as funções necessárias. Em seguida, usamos a função `openFile` para abrir um arquivo e especificamos o caminho, o modo de manipulação e o nome da ação que será realizada. Por exemplo:

```Haskell
file <- openFile "meuarquivo.txt" WriteMode
```

Agora, podemos usar a função `hPutStrLn` para escrever uma linha inteira no nosso arquivo:

```Haskell
hPutStrLn file "Este é um exemplo de texto"
```

Para escrever múltiplas linhas, podemos usar a função `hPutStr` e incluir caracteres de nova linha `\n` no final de cada linha. E, por fim, para fechar o arquivo, usamos a função `hClose`:

```Haskell
hClose file
```

## Mergulho Profundo

Além das funções mencionadas acima, o módulo `System.IO` também nos fornece outras ferramentas para trabalhar com arquivos. Podemos usar a função `hPutStr` para escrever uma string inteira, a função `hPutChar` para escrever um único caractere, e a função `hFlush` para descarregar qualquer dado que ainda não tenha sido gravado no arquivo.

Também podemos especificar diferentes modos de manipulação ao abrir o arquivo, como `WriteMode` para criar um novo arquivo ou sobrescrever um existente, `AppendMode` para adicionar conteúdo ao final de um arquivo e `ReadMode` para ler um arquivo existente.

## Veja Também

- [Documentação do módulo `System.IO`](https://hackage.haskell.org/package/base-4.12.0.0/docs/System-IO.html)
- [Exemplos de escrita de arquivos em Haskell](https://wiki.haskell.org/File_manipulation)