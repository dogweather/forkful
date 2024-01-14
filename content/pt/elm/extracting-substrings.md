---
title:    "Elm: Extraindo Substrings"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que extrair substrings no Elm é útil?

Extrair substrings é uma habilidade útil em qualquer linguagem de programação, e o Elm não é exceção. Essa funcionalidade permite que você manipule strings de maneira mais flexível, dividindo-as em partes menores para realizar ações específicas. Além disso, a extração de substrings pode ajudar a melhorar a eficiência do seu código, reduzindo o tempo de execução e o consumo de recursos.

## Como extrair substrings no Elm

Extrair substrings no Elm é bastante simples. Usando a função `String.slice`, você pode especificar o índice inicial e final do segmento de string que deseja extrair. Por exemplo:

```Elm
String.slice 0 5 "Olá Mundo"
```

O código acima retorna "Olá", pois estamos extraindo os caracteres da posição 0 até a posição 5 da string "Olá Mundo". Esta é a sintaxe básica para extrair substrings no Elm.

## Aprofundando na extração de substrings

A função `String.slice` também pode receber valores negativos como índices. Isso permite extrair substrings começando a partir do final da string. Além disso, você também pode usar a função `String.length` para obter o comprimento de uma string e usá-lo para definir os índices.

Outra função útil é `String.left`, que extrai os primeiros n caracteres de uma string, e `String.right` que extrai os últimos n caracteres. Essas funções são especialmente úteis quando combinadas com `String.length`, pois você pode definir o número de caracteres a serem extraídos dinamicamente.

Além disso, a biblioteca de strings do Elm possui outras funções úteis, como `String.split` e `String.trim`, que podem ser usadas em conjunto com a extração de substrings para realizar tarefas como dividir uma string em uma lista de palavras ou remover espaços em branco.

## Veja também

Aqui estão alguns links úteis para aprofundar seus conhecimentos sobre a extração de substrings no Elm:

- Documentação oficial da função `String.slice`: https://package.elm-lang.org/packages/elm-lang/core/latest/String#slice
- Um guia completo sobre manipulação de strings no Elm: https://www.elm-tutorial.org/pt/06-manipulating-strings.html
- Outros recursos úteis de programação em Elm: https://github.com/isRuslan/awesome-elm