---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:38.510109-07:00
description: "Express\xF5es regulares (regex) na programa\xE7\xE3o s\xE3o padr\xF5\
  es utilizados para combinar sequ\xEAncias de caracteres em strings. No Elm, assim\
  \ como em outras\u2026"
lastmod: '2024-02-25T18:49:44.109212-07:00'
model: gpt-4-0125-preview
summary: "Express\xF5es regulares (regex) na programa\xE7\xE3o s\xE3o padr\xF5es utilizados\
  \ para combinar sequ\xEAncias de caracteres em strings. No Elm, assim como em outras\u2026"
title: "Usando express\xF5es regulares"
---

{{< edit_this_page >}}

## O que & Por quê?
Expressões regulares (regex) na programação são padrões utilizados para combinar sequências de caracteres em strings. No Elm, assim como em outras linguagens, os programadores usam regex para tarefas como validar entrada, buscar e substituir texto dentro de strings devido à sua flexibilidade e eficiência.

## Como fazer:
Elm não possui funções de regex embutidas em sua biblioteca principal, o que requer o uso de bibliotecas de terceiros para essas operações. Uma das escolhas populares para trabalhar com regex é `elm/regex`. Você pode adicionar isto ao seu projeto usando `elm install elm/regex`.

Veja como você pode usar `elm/regex` para algumas tarefas comuns:

### 1. Correspondendo a um padrão
Para verificar se uma string corresponde a um padrão, você pode usar `Regex.contains`.

```elm
import Regex

pattern : Regex.Regex
pattern = Regex.fromString "^[a-zA-Z0-9]+$" |> Maybe.withDefault Regex.never

isAlphanumeric : String -> Bool
isAlphanumeric input = Regex.contains pattern input

-- Exemplo de uso:
isAlphanumeric "Elm2023"     -- Saída: True
isAlphanumeric "Elm 2023!"   -- Saída: False
```

### 2. Encontrando todas as correspondências
Para encontrar todas as ocorrências de um padrão dentro de uma string, você pode usar `Regex.find`.

```elm
matches : Regex.Regex
matches = Regex.fromString "\\b\\w+\\b" |> Maybe.withDefault Regex.never

getWords : String -> List String
getWords input = 
    input
        |> Regex.find matches
        |> List.map (.match)

-- Exemplo de uso:
getWords "Elm is fun!"  -- Saída: ["Elm", "is", "fun"]
```

### 3. Substituindo texto
Para substituir partes de uma string que correspondam a um padrão, você usa `Regex.replace`.

```elm
replacePattern : Regex.Regex
replacePattern = Regex.fromString "Elm" |> Maybe.withDefault Regex.never

replaceElmWithHaskell : String -> String
replaceElmWithHaskell input = 
    Regex.replace replacePattern (\_ -> "Haskell") input

-- Exemplo de uso:
replaceElmWithHaskell "Learning Elm is fun!"  
-- Saída: "Learning Haskell is fun!"
```

Nestes exemplos, `Regex.fromString` é usado para compilar um padrão regex, onde `\b` corresponde a limites de palavras e `\w` corresponde a qualquer caractere de palavra. Sempre trate o resultado `Maybe` de `Regex.fromString` para se proteger contra padrões regex inválidos, tipicamente usando `Maybe.withDefault`.
