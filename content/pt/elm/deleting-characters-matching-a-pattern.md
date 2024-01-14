---
title:    "Elm: Excluindo caracteres que correspondem a um padrão"
keywords: ["Elm"]
---

{{< edit_this_page >}}

##Por que

Às vezes, durante a programação em Elm, você pode se deparar com a necessidade de excluir caracteres que correspondam a um determinado padrão. Isso pode ser feito por uma variedade de razões, como limpar dados, filtrar informações ou realizar ações específicas com base nesses caracteres.

## Como fazer

Este processo é conhecido como "substituição de caracteres" e pode ser facilmente realizado usando o pacote `elm/regex`, que é amplamente utilizado para lidar com expressões regulares em Elm.

```Elm
import Regex exposing (replace, regex)

string = "Olá! Este é um exemplo de texto com números 123 e símbolos!@#$"

pattern = regex "[0-9!@#$]" -- Define o padrão de caracteres que serão excluídos

resultado = replace pattern string "" -- Substitui os caracteres correspondentes por uma string vazia

-- O resultado seria: "Olá Este é um exemplo de texto com números e símbolos"
```

Neste exemplo, criamos uma string de exemplo e um padrão que contém todos os caracteres que desejamos excluir. Em seguida, usamos a função `replace` para substituir esses caracteres por uma string vazia, resultando em uma nova string sem os caracteres indesejados.

## Aprofundando

Além de simplesmente substituir caracteres correspondentes por uma string vazia, o pacote `elm/regex` oferece uma ampla variedade de funções úteis para trabalhar com expressões regulares em Elm. Isso inclui encontrar correspondências, dividir strings em partes e substituir correspondências por outras strings.

Para saber mais sobre como trabalhar com expressões regulares em Elm, verifique a documentação oficial do pacote [aqui](https://package.elm-lang.org/packages/elm/regex/latest/Regex) e o guia simples e prático [RegexOne](https://regexone.com/), que pode ser adaptado facilmente para o uso em Elm.

## Veja também

- [Documentação oficial do pacote elm/regex](https://package.elm-lang.org/packages/elm/regex/latest/Regex)
- [Guia RegexOne para expressões regulares](https://regexone.com/)