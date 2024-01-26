---
title:                "Escrevendo um arquivo de texto"
html_title:           "Arduino: Escrevendo um arquivo de texto"
simple_title:         "Escrevendo um arquivo de texto"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## O que é & Por quê?
Escrever em um arquivo de texto é o processo de salvar dados em um arquivo no disco. Programadores fazem isso para persistir dados, configurar aplicações, ou logar informações.

## Como fazer:
```Haskell
import System.IO

-- Escrevendo em um arquivo
main :: IO ()
main = do
    let lista = ["linha 1", "linha 2", "linha 3"]
    writeFile "arquivo.txt" (unlines lista)
```
Saída (conteúdo de "arquivo.txt"):
```
linha 1
linha 2
linha 3
```

## Mergulho Profundo
Haskell vem com suporte embutido para escrita de arquivos desde suas versões iniciais. Alternativas incluem o uso de bibliotecas como `text` e `bytestring` para melhor performance ou funcionalidades adicionais. Detalhes de implementação envolvem tratar de maneira eficiente a escrita no disco e garantir o fechamento adequado dos arquivos após a escrita.

## Veja Também
- Documentação da biblioteca `System.IO` de Haskell para operações de entrada e saída de arquivo: [System.IO - Haskell](https://hackage.haskell.org/package/base/docs/System-IO.html)
- Um guia para a leitura e escrita de arquivos no Haskell Wiki: [Haskell IO Tutorial](https://wiki.haskell.org/IO_inside)
- Pacote `text` para trabalhar com texto Unicode em Haskell: [text - Hackage](https://hackage.haskell.org/package/text)
- Pacote `bytestring` para trabalhar com sequências de bytes: [bytestring - Hackage](https://hackage.haskell.org/package/bytestring)
