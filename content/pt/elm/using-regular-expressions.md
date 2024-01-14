---
title:                "Elm: Usando expressões regulares"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por que usar Expressões Regulares em Elm?

As expressões regulares são uma poderosa ferramenta usada para manipulação de texto e são amplamente utilizadas em diversas linguagens de programação, incluindo Elm. Com elas, é possível realizar buscas, substituições e validações de padrões em texto de forma simples e eficiente. Em Elm, é possível utilizar expressões regulares através da biblioteca `elm/regex`, que fornece funções e tipos de dados específicos para trabalhar com elas.

## Como usar Expressões Regulares em Elm

Para utilizar expressões regulares em Elm, primeiro é necessário importar a biblioteca `elm/regex` em nosso código. Em seguida, podemos criar um objeto Regex utilizando a função `Regex.fromString`, passando como argumento uma string contendo a expressão regular desejada. Podemos, então, utilizar os métodos proporcionados por essa biblioteca para realizar as operações desejadas.

```Elm
import Regex exposing (..)

-- Criando uma expressão regular para buscar email
regexEmail = Regex.fromString "[a-zA-Z0-9_.]+@[a-z]+\\.[a-z]+"

-- Realizando uma busca em uma string
Regex.contains regexEmail "meuemail@exemplo.com" // True
Regex.contains regexEmail "meuemail.com" // False

-- Substituindo texto utilizando expressões regulares
Regex.replace regexEmail (\_ -> "Endereço de email inválido") "meuemailcom" // "Endereço de email inválido"
```

## Aprofundando no uso de Expressões Regulares em Elm

No exemplo acima, utilizamos uma expressão regular simples para buscar e validar um endereço de email. Porém, as expressões regulares possuem uma sintaxe complexa e poderosa, permitindo a criação de padrões mais específicos. É possível utilizar caracteres especiais, quantificadores, grupos e muito mais para construir expressões regulares mais precisas e eficientes.

Além disso, em Elm é possível utilizar `Regex.replace` para realizar substituições múltiplas em uma única string, passando uma função como segundo argumento. Essa função receberá o resultado da busca e poderá retornar o texto substituto, permitindo, por exemplo, a correção automática de erros ortográficos.

## Veja também
- Documentação oficial do pacote `elm/regex`: https://package.elm-lang.org/packages/elm/regex/latest/
- Tutorial sobre expressões regulares em Elm: https://guide.elm-lang.org/interop/regex.html
- Exemplos de uso de expressões regulares em Elm: https://github.com/elm-community/regex-examples