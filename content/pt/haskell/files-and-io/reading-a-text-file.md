---
title:                "Lendo um arquivo de texto"
aliases:
- /pt/haskell/reading-a-text-file/
date:                  2024-01-20T17:54:23.433679-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lendo um arquivo de texto"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## O Que é & Por Que?

Ler um arquivo de texto significa acessar o conteúdo armazenado. Programadores fazem isso para manipular, analisar ou exibir dados.

## Como Fazer:

```haskell
import System.IO  

main :: IO ()
main = do  
    conteudo <- readFile "meuArquivo.txt"
    putStrLn conteudo
```

Saída esperada (depende do conteúdo do seu `meuArquivo.txt`):
```
Olá, mundo!
Este é o conteúdo do meu arquivo de texto.
```

## Mergulho Profundo:

Ler arquivos no Haskell é tratado de forma pura usando a monad `IO`. Historicamente, isso permite que Haskell mantenha sua pureza funcional e lide com efeitos colaterais como a leitura de arquivos. Alternativas ao `readFile` incluem `hGetContents` junto com funções que dão controle mais fino como `openFile`, `hSetEncoding`, e `hClose`. A implementação está apoiada em `laziness`, o que significa que o arquivo é lido por demanda – peça por peça – o que é eficiente em termos de memória.

## Veja Também:

- Documentação oficial de Haskell: [Haskell.org](https://www.haskell.org/documentation/)
- Livro "Learn You a Haskell for Great Good!" para uma introdução agradável e profunda à linguagem: [Learn You a Haskell](http://learnyouahaskell.com/)
- Haskell Wiki sobre I/O: [Haskell Wiki](https://wiki.haskell.org/IO_inside)
