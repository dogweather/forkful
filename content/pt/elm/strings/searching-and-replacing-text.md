---
date: 2024-01-20 17:57:29.981247-07:00
description: "Como Fazer: Substituir texto \xE9 uma opera\xE7\xE3o comum desde os\
  \ prim\xF3rdios da programa\xE7\xE3o. Op\xE7\xF5es incluem express\xF5es regulares,\
  \ mas no Elm atual, usamos\u2026"
lastmod: '2024-04-05T21:53:46.824105-06:00'
model: gpt-4-1106-preview
summary: "Substituir texto \xE9 uma opera\xE7\xE3o comum desde os prim\xF3rdios da\
  \ programa\xE7\xE3o."
title: Pesquisando e substituindo texto
weight: 10
---

## Como Fazer:
```Elm
import String

-- Função simples para procurar e substituir texto
substituirTexto : String -> String -> String -> String
substituirTexto de para texto =
    String.split de texto |> String.join para

-- Exemplo de uso
main =
    "Olá, mundo!" |> substituirTexto "mundo" "todos"

-- Saída esperada: "Olá, todos!"
```

## Aprofundamento
Substituir texto é uma operação comum desde os primórdios da programação. Opções incluem expressões regulares, mas no Elm atual, usamos `String.split` seguido de `String.join` para um método mais direto e seguro. Alternativas como bibliotecas de terceiros existem, mas muitas vezes a solução padrão da linguagem é suficiente e recomendada para evitar complexidade e dependências extras.

## Veja Também
- Documentação oficial do Elm para strings: https://package.elm-lang.org/packages/elm/core/latest/String
- Elm Guide sobre a manipulação de strings: https://guide.elm-lang.org/strings/
- Tutorial sobre expressões regulares (não específico para Elm, mas útil para entender o conceito): https://regexr.com/
