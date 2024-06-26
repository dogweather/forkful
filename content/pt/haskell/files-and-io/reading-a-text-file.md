---
date: 2024-01-20 17:54:23.433679-07:00
description: "Como Fazer: Sa\xEDda esperada (depende do conte\xFAdo do seu `meuArquivo.txt`)."
lastmod: '2024-04-05T21:53:46.988001-06:00'
model: gpt-4-1106-preview
summary: "Sa\xEDda esperada (depende do conte\xFAdo do seu `meuArquivo.txt`)."
title: Lendo um arquivo de texto
weight: 22
---

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
