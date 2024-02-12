---
title:                "Pesquisando e substituindo texto"
aliases:
- /pt/elm/searching-and-replacing-text.md
date:                  2024-01-20T17:57:29.981247-07:00
model:                 gpt-4-1106-preview
simple_title:         "Pesquisando e substituindo texto"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Procurar e substituir texto é trocar uma sequência específica por outra em uma string. Programadores fazem isso para corrigir dados, atualizar informações ou manipular texto de maneira eficiente.

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
