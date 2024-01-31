---
title:                "Escrevendo no erro padrão"
date:                  2024-01-19
html_title:           "Arduino: Escrevendo no erro padrão"
simple_title:         "Escrevendo no erro padrão"

category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## O Quê & Porquê?
Escrever no erro padrão (*standard error*) é a prática de enviar mensagens de erro para um stream específico, separado da saída padrão (*standard output*). Isso permite aos programadores diferenciar saída normal de mensagens de erro, e é essencial para diagnóstico e depuração.

## Como Fazer:
```Haskell
import System.IO

main :: IO ()
main = do
    hPutStrLn stderr "Isto é uma mensagem de erro."
    putStrLn "Isto é uma saída normal."

-- Saída esperada:
-- No console (stderr):
-- Isto é uma mensagem de erro.
-- No console (stdout):
-- Isto é uma saída normal.
```

## Aprofundamento
O erro padrão é um canal de output que existe desde os primórdios dos sistemas Unix, criado para que programas possam comunicar problemas sem interferir na saída de dados. Alternativas incluem a escrita em arquivos de log ou o uso de bibliotecas para lidar com mensagens de erro. Este último pode dar mais controle e opções de formatação, mas escrever diretamente no erro padrão é prático para scripts rápidos ou programas menores. Ao implementar essa funcionalidade em Haskell, usamos a biblioteca `System.IO` que provê a abstração necessária para trabalhar com vários streams de input/output, de forma a manter o código claro e conciso.

## Veja Também:
- [Haskell Documentation on System.IO](https://hackage.haskell.org/package/base-4.16.0.0/docs/System-IO.html)
- [Learn You a Haskell for Great Good! on Input and Output](http://learnyouahaskell.com/input-and-output)
- [Real World Haskell – Chapter 7: I/O](http://book.realworldhaskell.org/read/io.html)
