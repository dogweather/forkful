---
title:                "Haskell: Escrevendo no erro padrão"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que escrever para o padrão de erro?

Escrever para o padrão de erro em Haskell pode ser extremamente útil para os programadores. Isso permite que os erros sejam exibidos de uma maneira mais clara e legível, facilitando a detecção e resolução de bugs em seus códigos.

## Como fazer

Para escrever para o padrão de erro em Haskell, você precisará importar o módulo "System.IO" e usar a função "hPutStrLn" para imprimir sua mensagem de erro. Veja um exemplo abaixo:

```Haskell
import System.IO

main = do
  hPutStrLn stderr "Ocorreu um erro: Não foi possível encontrar o arquivo."
```

A saída do código acima será algo como: "Ocorreu um erro: Não foi possível encontrar o arquivo." na saída de erro.

## Profundidade

Além da função "hPutStrLn", existem outras funções úteis que podem ser usadas para escrever em padrão de erro em Haskell, como "hPutStr" e "hPutChar". Além disso, é importante ter em mente que o padrão de erro é um fluxo de saída diferente do padrão de entrada e de saída, portanto, é preciso ter cuidado ao manipulá-lo em seus códigos.

## Veja também

- Documentação do módulo "System.IO" em Haskell: https://hackage.haskell.org/package/base/docs/System-IO.html
- Tutorial básico sobre escrita para o padrão de erro em Haskell: https://www.haskell.org/tutorial/io.html