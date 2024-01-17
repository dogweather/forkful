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

## O que & Por que?
Ler um arquivo de texto é simplesmente ler o conteúdo de um arquivo de texto, como o próprio nome sugere. Os programadores muitas vezes precisam ler arquivos de texto para obter dados que serão usados em seus programas.

## Como:
```Haskell
import System.IO

main = do
    contents <- readFile "texto.txt"
    putStr contents
```

Saída:
```
Este é um texto de exemplo
para demonstrar como ler
um arquivo de texto em Haskell.
```

## Detalhes:
Historicamente, a leitura de arquivos de texto era necessária para processar informações em um computador. Hoje, existem alternativas, como o armazenamento em banco de dados, mas ler arquivos de texto ainda é uma tarefa comum em programação. A implementação específica da leitura de arquivos pode variar dependendo da linguagem de programação, mas o conceito geral é o mesmo.

## Veja também:
- [Documentação oficial do Haskell](https://www.haskell.org/documentation/)
- [Tutorial para iniciantes em Haskell](http://learnyouahaskell.com/)
- [Exemplos de código em Haskell](https://www.rosettacode.org/wiki/Category:Haskell)