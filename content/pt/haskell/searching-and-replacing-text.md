---
title:                "Procurando e substituindo texto."
html_title:           "Haskell: Procurando e substituindo texto."
simple_title:         "Procurando e substituindo texto."
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que

Você pode precisar realizar uma busca e substituição de texto quando estiver trabalhando com grandes quantidades de dados ou precisar fazer mudanças em um texto extenso de forma eficiente. O Haskell oferece uma maneira poderosa e elegante de lidar com essa tarefa através do uso de funções e expressões de alta ordem.

## Como Fazer

Aqui estão alguns exemplos simples de como realizar uma busca e substituição de texto em Haskell:

```Haskell
-- Substituindo todas as ocorrências de "olá" por "oi" em uma string
replaceText "olá" "oi" "olá, tudo bem?"
-- Resultado: "oi, tudo bem?"

-- Utilizando expressões lambda para realizar uma substituição mais complexa
replaceText "incluído" (\_ -> "adicionado") "Foi incluído um novo recurso."
-- Resultado: "Foi adicionado um novo recurso."

-- Realizando uma substituição em uma lista de strings
map (replaceText " app " " aplicativo ") ["Este é um ótimo app!", "Meu app favorito."]
-- Resultado: ["Este é um ótimo aplicativo!", "Meu aplicativo favorito."]
```

## Profundidade

A função `replaceText` utilizada nos exemplos acima é um exemplo de uma função de alta ordem em Haskell, que recebe outras funções como argumentos. Essa abordagem funcional permite uma maior flexibilidade e extensibilidade no processo de busca e substituição de texto.

Além disso, em Haskell, as strings são tratadas como listas de caracteres, o que facilita a manipulação e processamento de texto. Uma biblioteca popular para trabalhar com strings em Haskell é o `Data.Text`, que oferece funções otimizadas para lidar com grandes quantidades de texto.

## Veja Também

- [Documentação da função `replaceText` do módulo `Data.Text` em Haskell](https://hackage.haskell.org/package/text/docs/Data-Text.html#g:4)
- [Tutorial de Haskell no Wikibooks](https://pt.wikibooks.org/wiki/Haskell)