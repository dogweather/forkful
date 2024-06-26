---
date: 2024-01-20 17:58:02.773936-07:00
description: "Como Fazer: Substituir texto \xE9 uma opera\xE7\xE3o t\xE3o antiga quanto\
  \ os primeiros editores de texto. Em Haskell, a biblioteca `Data.Text` \xE9 uma\
  \ alternativa\u2026"
lastmod: '2024-04-05T22:50:59.867287-06:00'
model: gpt-4-1106-preview
summary: "Substituir texto \xE9 uma opera\xE7\xE3o t\xE3o antiga quanto os primeiros\
  \ editores de texto."
title: Pesquisando e substituindo texto
weight: 10
---

## Como Fazer:
```Haskell
import Data.Text as T

-- Exemplo de função de substituição de texto
substituirTexto :: T.Text -> T.Text -> T.Text -> T.Text
substituirTexto antigo novo = T.replace antigo novo

main :: IO ()
main = do
  let textoOriginal = "Olá mundo! Programação em Haskell é demais!"
  let textoAtualizado = substituirTexto "mundo" "galera" textoOriginal
  putStrLn $ T.unpack textoAtualizado
```

Saída da amostra:
```
Olá galera! Programação em Haskell é demais!
```

## Mergulho Profundo
Substituir texto é uma operação tão antiga quanto os primeiros editores de texto. Em Haskell, a biblioteca `Data.Text` é uma alternativa moderna às Strings tradicionais (`[Char]`), oferecendo melhor desempenho e funcionalidades prontas como `replace`. Além disso, existem bibliotecas de expressões regulares como `regex-tdfa` se as substituições forem mais complexas.

Implementar uma função de substituição eficiente em Haskell pode ser um desafio, porque strings em Haskell são imutáveis. Isso significa que modificar um caractere envolve a criação de uma nova string. No entanto, `Data.Text` utiliza arrays mutáveis internamente para otimizar operações como substituição.

## Veja Também
- [Text](https://hackage.haskell.org/package/text) – Documentação sobre a biblioteca Text.
- [Tutorial básico de Haskell](http://haskell.tailorfontela.com.br/) – Um guia prático e introdutório em português.
- [Regular Expressions: regex-tdfa](https://hackage.haskell.org/package/regex-tdfa) – Para substituições baseadas em padrões complexos.
